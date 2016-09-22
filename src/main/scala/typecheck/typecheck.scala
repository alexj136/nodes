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

  def constraintsExp(
    e: Exp,
    env: Map[Name, SType],
    nn: Name
  ): (SType, Set[(SType, SType)], Name) =
    e match {
      case Variable    ( name          ) => (env(name), Set.empty, nn)
      case IntLiteral  ( value         ) => (SInt     , Set.empty, nn)
      case BoolLiteral ( value         ) => (SBool    , Set.empty, nn)
      case ChanLiteral ( name          ) => (SChan    , Set.empty, nn)
      case Pair        ( l    , r      ) => {
        val (tyL, constrL, nnL): (SType, Set[(SType, SType)], Name) =
          constraintsExp(l, env, nn)
        val (tyR, constrR, nnR): (SType, Set[(SType, SType)], Name) =
          constraintsExp(r, env, nnL)
        (SPair(tyL, tyR), constrL union constrR, nnR)
      }
      case UnExp       ( op   , of     ) => {
        val (tyOf, constrOf, nnOf): (SType, Set[(SType, SType)], Name) =
          constraintsExp(of, env, nn)
        val (tyOp, nnOp): (SType, Name) = dequantify(typeOfUnOp(op), nnOf)
        (returnType(tyOp), constrOf union Set((argumentType(tyOp), tyOf)), nnOp)
      }
      case BinExp      ( op   , l  , r ) => ???
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

  /**
   * Given an SType assumed to be a function type (as would always be after
   * dequantification of a UnOp or BinOp), get the return type of the function.
   */
  def returnType(ty: SType): SType = ty match {
    case SFunc(_, r) => r
    case _           => {
      val errMsg: String =
        "tried to get the return type of a non-SFunc SType. returnType() may " +
        "only be called on STypes known to be SFunc, such as those returned "  +
        "from a call to dequantify()."
      throw new RuntimeException(errMsg)
    }
  }

  /**
   * Given an SType assumed to be a function type (as would always be after
   * dequantification of a UnOp or BinOp), get the argument type of the
   * function.
   */
  def argumentType(ty: SType): SType = ty match {
    case SFunc(a, _) => a
    case _           => {
      val errMsg: String =
        "tried to get the argument type of a non-SFunc SType. argumentType() " +
        "may only be called on STypes known to be SFunc, such as those "       +
        "returned from a call to dequantify()."
      throw new RuntimeException(errMsg)
    }
  }

  /**
   * Remove quantifiers from an SType. The Name parameter & Name return value is
   * the next globally available Name, before and after the dequantification.
   */
  def dequantify(ty: SType, nn: Name): (SType, Name) = ty match {
    case SInt            => (SInt   , nn)
    case SBool           => (SBool  , nn)
    case SChan           => (SChan  , nn)
    case SVar  ( n     ) => (SVar(n), nn)
    case SPair ( l , r ) => {
      val (deL, nnL): (SType, Name) = dequantify(l, nn)
      val (deR, nnR): (SType, Name) = dequantify(r, nnL)
      (SPair(deL, deR), nnR)
    }
    case SFunc ( a , r ) => {
      val (deA, nnA): (SType, Name) = dequantify(a, nn)
      val (deR, nnR): (SType, Name) = dequantify(r, nnA)
      (SFunc(deA, deR), nnR)
    }
    case SQuant( n , t ) => {
      val (newT, nnT): (SType, Name) = subst(t, n, nn, nn.next)
      dequantify(newT, nnT)
    }
  }

  /**
   * Substitute type variables in a type expression, of a given name, to another
   * given name. Alpha conversion of quantified expressions is sometimes
   * necessary to prevent erroneous capture, so a next-name parameter is
   * required and returned.
   */
  def subst(ty: SType, from: Name, to:Name, nn: Name): (SType, Name) =
    ty match {
      case SInt            => (SInt                           , nn)
      case SBool           => (SBool                          , nn)
      case SChan           => (SChan                          , nn)
      case SVar  ( n     ) => (SVar(if (n == from) to else n) , nn)
      case SPair ( l , r ) => {
        val (subL, nnL): (SType, Name) = subst(l, from, to, nn)
        val (subR, nnR): (SType, Name) = subst(r, from, to, nnL)
        (SPair(subL, subR), nnR)
      }
      case SFunc ( a , r ) => {
        val (subA, nnA): (SType, Name) = subst(a, from, to, nn)
        val (subR, nnR): (SType, Name) = subst(r, from, to, nnA)
        (SFunc(subA, subR), nnR)
      }
      case SQuant( n , t ) =>
        if (n == from) (SQuant(n, t), nn)
        // If substituting within the quantifier would cause this quantifier to
        // erroneously capture the substituted name, first rename the quantifier
        // and its bound variables to a new name.
        else if (n == to) {
          val (subT, nnT): (SType, Name) = subst(t, n, nn, nn.next)
          val (subSubT, nnST): (SType, Name) = subst(subT, from, to, nnT)
          (SQuant(nn, subSubT), nnST)
        }
        else {
          val (subT, nnT): (SType, Name) = subst(t, from, to, nn)
          (SQuant(n, subT), nnT)
        }
    }
}
