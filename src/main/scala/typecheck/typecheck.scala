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
    case SQuant( n , t ) => dequantify(subst(t, n, nn), nn.next)
  }

  /**
   * Substitute type variables in a type expression, of a given name, to another
   * given name.
   */
  def subst(ty: SType, from: Name, to:Name): SType = ty match {
    case SInt            => SInt
    case SBool           => SBool
    case SChan           => SChan
    case SPair ( l , r ) => SPair(subst(l, from, to), subst(r, from, to))
    case SVar  ( n     ) => SVar(if (n == from) to else n)
    case SFunc ( a , r ) => SFunc(subst(a, from, to), subst(r, from, to))
    case SQuant( n , t ) =>
      if (n == from) SQuant(n, t)
      // If substituting within the quantifier would cause this quantifier to
      // erroneously capture the substituted name, first rename the quantifier
      // and its bound variables to a new name.
      else if (n == to) {
        val newN: Name = maxOfConcreteAndOptionalName(n, maxSVarName(t)).next
        SQuant(newN, subst(subst(t, n, newN), from, to))
      }
      else SQuant(n, subst(t, from, to))
  }

  /**
   * Get the maximum SVar name in a type. Returns an Option[Name] because a
   * type may not contain any variables and thus have no maximum name.
   */
  def maxSVarName(ty: SType): Option[Name] = ty match {
    case SInt            => None
    case SBool           => None
    case SChan           => None
    case SPair ( l , r ) => maxOf(maxSVarName(l), maxSVarName(r))
    case SVar  ( n     ) => Some(n)
    case SQuant( n , t ) => maxOf(Some(n), maxSVarName(t))
    case SFunc ( a , r ) => maxOf(maxSVarName(a), maxSVarName(r))
  }

  /**
   * Out of two Option[Name]s, optionally compute the greater one. If one is
   * None, the other is greater. If both are None, the result is None.
   */
  def maxOf(a: Option[Name], b: Option[Name]): Option[Name] = (a, b) match {
    case (Some(na) , Some(nb)) => if (na.id >= nb.id) a else b
    case (Some(_)  , None    ) => a
    case (None     , Some(_) ) => b
    case (None     , None    ) => None
  }

  /**
   * Out of a Name and an Option[Name], compute which one is greater. If the
   * optional one is None, the non-optional one is greater by default.
   */
  def maxOfConcreteAndOptionalName(n: Name, on: Option[Name]): Name = on match {
    case None    => n
    case Some(m) => if (n.id >= m.id) n else m
  }
}
