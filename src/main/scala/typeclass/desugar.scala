package typeclass.desugar

import syntax._
import typeclass.syntax._

sealed abstract class DesugarError
case class FreeClassNameError ( n: Name ) extends DesugarError

case class ClassInfo
  ( val classBind : Name
  , val classTy   : SChan
  , val instances : Map[SType, Receive]
  ) {
  def withInstance(ty: SType, witness: Receive): ClassInfo =
    ClassInfo(classBind, classTy, instances + (ty -> witness))
}

class Desugar ( nextName: NumName ) {

  private var nn: NumName = nextName

  def fresh: NumName = { val tmp: NumName = nn ; this.nn = this.nn.next ; tmp }

  def desugarTypeClassElement(info: Map[Name, ClassInfo],
      elem: TypeClassElement): Either[DesugarError, Proc] = elem match {

    case TypeClassDecl(tcName, bind, ty, e) => {
      val tcInfo: ClassInfo = ClassInfo(bind, ty, Map())
      desugarTypeClassElement(info + (tcName -> tcInfo), e)
    }
    case TypeClassInst(tcName, ty, witness, e) if info contains tcName => {
      val tcInfo: ClassInfo = info(tcName).withInstance(ty, witness)
      desugarTypeClassElement(info + (tcName -> tcInfo), e)
    }
    case TypeClassInst(tcName, _, _, _) => Left(FreeClassNameError(tcName))
    case TypeClassProc(p) => Right(desugarProc(info, Map.empty, p))
  }

  def desugarProc(
    info: Map[Name, ClassInfo],
    env:  Map[Name, SType],
    p:    Proc
  ): Proc = {
    def desugarE(e: Exp ): Exp  = desugarExp (info, env, e)
    def desugarP(p: Proc): Proc = desugarProc(info, env, p)
    p match {
      case Send       (ch   , ts , ms  , q      ) =>
        Send(desugarE(ch), ts, ms, desugarP(q))
      case Receive    (r    , ch , qs  , as , q ) =>
        Receive(r, desugarE(ch), qs, as, desugarP(q))
      case LetIn      (bind , t  , exp , q      ) =>
        LetIn(bind, t, desugarE(exp), desugarP(q))
      case IfThenElse (exp  , tq , fq           ) =>
        IfThenElse(desugarE(exp), desugarP(tq), desugarP(fq))
      case Parallel   (q    , r                 ) =>
        Parallel(desugarP(q), desugarP(r))
      case New        (name , ty , q            ) =>
        New(name, ty, desugarP(q))
      case End                                    => End
    }
  }

  def desugarExp(
    info: Map[Name, ClassInfo],
    env:  Map [Name, SType],
    e:    Exp
  ): Exp = {
    def desugar(e: Exp): Exp = desugarExp(info, env, e)
    e match {
      case Variable    ( _         ) => ???
      case IntLiteral  ( _         ) => e
      case BoolLiteral ( _         ) => e
      case ChanLiteral ( _         ) => e
      case KharLiteral ( _         ) => e
      case Pair        ( l , r     ) => Pair(desugar(l), desugar(r))
      case UnExp       ( o , a     ) => UnExp(o, desugar(a))
      case BinExp      ( o , l , r ) => BinExp(o, desugar(l), desugar(r))
      case ListExp     ( es        ) => ListExp(es map (desugar(_)))
    }
  }
}
