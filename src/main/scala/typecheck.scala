package typecheck

import syntax._

sealed abstract class SType
case object SInt extends SType
case object SBool extends SType
case object SChan extends SType
case class SPair(l: SType, r: SType) extends SType
case class SVar(n: Name) extends SType
case class SQuant(n: Name, t: SType) extends SType
case class SFunc(a: SType, r: SType) extends SType

object Typecheck {

  def checkProc(p: Proc, env: Map[Name, SType]): SType = ???

  def checkExp(e: Exp, env: Map[Name, SType]): SType = e match {
    case Variable    ( name          ) => env(name)
    case IntLiteral  ( value         ) => SInt
    case BoolLiteral ( value         ) => SBool
    case ChanLiteral ( name          ) => SChan
    case Pair        ( l    , r      ) =>
      SPair(checkExp(l, env), checkExp(r, env))
    case UnExp       ( ty   , of     ) => ???
    case BinExp      ( ty   , l  , r ) => ???
  }

  def typeOfBinOp(op: BinOp): SType = op match {
    case Add        => SFunc(SInt , SFunc(SInt , SInt ))
    case Sub        => SFunc(SInt , SFunc(SInt , SInt ))
    case Mul        => SFunc(SInt , SFunc(SInt , SInt ))
    case Div        => SFunc(SInt , SFunc(SInt , SInt ))
    case Mod        => SFunc(SInt , SFunc(SInt , SInt ))
    case Equal      => SFunc(SInt , SFunc(SInt , SBool))
    case NotEqual   => SFunc(SInt , SFunc(SInt , SBool))
    case Less       => SFunc(SInt , SFunc(SInt , SBool))
    case LessEq     => SFunc(SInt , SFunc(SInt , SBool))
    case Greater    => SFunc(SInt , SFunc(SInt , SBool))
    case GreaterEq  => SFunc(SInt , SFunc(SInt , SBool))
    case And        => SFunc(SBool, SFunc(SBool, SBool))
    case Or         => SFunc(SBool, SFunc(SBool, SBool))
  }

  def typeOfUnOp(op: UnOp): SType = op match {
    case Not    => SFunc(SBool, SBool)
    case PLeft  => SQuant(new Name(0), SQuant(new Name(1),
      SFunc(SPair(SVar(new Name(0)), SVar(new Name(1))), SVar(new Name(0)))))
    case PRight => SQuant(new Name(0), SQuant(new Name(1),
      SFunc(SPair(SVar(new Name(0)), SVar(new Name(1))), SVar(new Name(1)))))
  }
}
