package typeclass.desugar

import syntax._
import typecheck._
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

object desugarTypeClassElement extends Function3
  [ NumName
  , Map[Name, ClassInfo]
  , TypeClassElement
  , Either[DesugarError, (NumName, Proc)]
  ] {
  def apply(nn: NumName, info: Map[Name, ClassInfo], elem: TypeClassElement):
      Either[DesugarError, (NumName, Proc)] = elem match {
    case TypeClassDecl(tcName, bind, ty, e) => {
      val tcInfo: ClassInfo = ClassInfo(bind, ty, Map())
      desugarTypeClassElement(nn, info + (tcName -> tcInfo), e)
    }
    case TypeClassInst(tcName, ty, witness, e) if info contains tcName => {
      val tcInfo: ClassInfo = info(tcName).withInstance(ty, witness)
      desugarTypeClassElement(nn, info + (tcName -> tcInfo), e)
    }
    case TypeClassInst(tcName, _, _, _) => Left(FreeClassNameError(tcName))
    case TypeClassProc(p) => Right(desugarProc(nn, info, p))
  }
}

object desugarProc extends Function3
  [ NumName
  , Map[Name, ClassInfo]
  , Proc
  , (NumName, Proc)
  ] {
  def apply(nn: NumName, info: Map[Name, ClassInfo], p: Proc):
      (NumName, Proc) = p match {
    case _ => ???
  }
}
