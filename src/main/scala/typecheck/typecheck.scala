package typecheck

import syntax._

case class ConstraintSet( val set: Set [ Constraint ] ) {

  def split: ( Option [ Constraint ] , ConstraintSet ) =
    set.toList match {
      case ( c :: cs ) => ( Some ( c ) , ConstraintSet( cs.toSet ) )
      case Nil         => ( None       , this                      )
    }

  def isEmpty: Boolean = set.isEmpty

  def union ( other: ConstraintSet ) : ConstraintSet =
    ConstraintSet ( set union other.set )

  def + ( constr: Constraint ) : ConstraintSet =
    if ( set contains constr )
      ConstraintSet ( set map ( _ combineIfCompatible constr ) )
    else
      ConstraintSet ( set + constr )

  def ++ ( constrs: List [ Constraint ] ) : ConstraintSet = constrs match {
    case Nil     => this
    case c :: cs => (this + c) ++ cs
  }

  def map ( f: SType => SType ) : ConstraintSet =
    ConstraintSet( set map ( _ map f ) )

  override def toString: String = "ConstraintSet (\n" +
    ( set.toList.map ( "  " + _ + "\n" ).foldLeft ( "" ) ( _ + _ ) ) + " )"

  def foreach ( f: Constraint => Unit ): Unit = set.foreach ( f )

  def size: Int = set.size
}

case object ConstraintSet {
  def empty: ConstraintSet = ConstraintSet ( Set.empty )
}

sealed abstract class Constraint ( val origins: List [ SyntaxElement ] ) {

  def trivial: Boolean

  def map ( f: SType => SType ): Constraint

  def pstr ( nameMap: Map [ Name , String ] ): String

  def combineIfCompatible ( other: Constraint ): Constraint
}

case class TypeConstraint
  ( val t1: SType
  , val t2: SType
  , orig: List [ SyntaxElement ]
  ) extends Constraint ( orig ) {

  def asPair: ( SType , SType ) = ( t1 , t2 )

  override def equals ( other: Any ): Boolean =
    if ( other.isInstanceOf [ TypeConstraint ] ) {
      val oc: TypeConstraint = other.asInstanceOf[TypeConstraint]
      ((this.t1 == oc.t1) && (this.t2 == oc.t2)) ||
      ((this.t2 == oc.t1) && (this.t1 == oc.t2))
    }
    else false

  def trivial: Boolean = t1 == t2

  def map ( f: SType => SType ): Constraint =
    TypeConstraint ( f ( t1 ) , f ( t2 ) , origins )

  def pstr ( nameMap: Map [ Name , String ] ): String =
    s"Type constraint $t1 == $t2 from:\n" +
      ( origins map ( e => s"${e.pstr(nameMap)}\n    at ${e.info}" ) ).toString

  def combineIfCompatible ( other: Constraint ): Constraint =
    if ( ! ( other equals this ) ) this else
      TypeConstraint ( this.t1 , this.t2 , this.origins ++ other.origins )
}

case class ArityConstraint
  ( val a1: Int
  , val a2: Int
  , orig: List [ SyntaxElement ]
  ) extends Constraint ( orig ) {

  def trivial: Boolean = a1 == a2

  def map ( f: SType => SType ): Constraint = this

  def pstr ( nameMap: Map [ Name , String ] ): String =
    s"Arity constraint from:\n" +
      ( origins map ( e => s"${e.pstr(nameMap)}\n    at ${e.info}" ) ).toString

  def combineIfCompatible ( other: Constraint ): Constraint = this
}

class Typecheck ( nextName: NumName ) {

  private var nn: NumName = nextName

  def fresh: NumName = { val freshN: NumName = nn ; nn = nn.next ; freshN }

  def checkProc(p: Proc): Option[SType] = {
    val (tyP: SType, constrP: ConstraintSet) = constraintsProc(p, Map.empty)
    unify (constrP, ConstraintSet.empty) match {
      case Right(unifyFn) => Some(unifyFn(tyP))
      case Left (_      ) => None
    }
  }

  /**
   * Constraint set unification. If a constraint is unsolvable, we add it to a
   * set of unsolvable constraints, and continue substitution within those
   * failed constraints. This allows for informative error messages.
   */
  def unify
    ( constrs: ConstraintSet
    , failed:  ConstraintSet
    ): Either [ ConstraintSet , SType => SType ] = {
    val exception = new RuntimeException (
      "SQuant not removed from type before unification" )
    constrs.split match {
      case (None, _) => if (failed.isEmpty) Right(identity) else Left(failed)
      case (Some(c), rest) if c.trivial => unify(rest, failed)
      case (Some(c), rest) if c.isInstanceOf[ArityConstraint] =>
        unify (rest, failed + c)
      case (Some(c), rest) if c.isInstanceOf[TypeConstraint] =>
        c.asInstanceOf[TypeConstraint].asPair match {

          case (SVar(n, _), ty     ) if !(ty free n) =>
            val subFn: SType => SType = _ sTypeSubst(n, ty)
            unify (rest map subFn, failed map subFn)
              .right.map(_ compose subFn)

          case (ty     , SVar(n, _)) if !(ty free n) =>
            val subFn: SType => SType = _ sTypeSubst(n, ty)
            unify (rest map subFn, failed map subFn)
              .right.map(_ compose subFn)

          case (SList(t1o), SList(t2o)) =>
            unify(rest + TypeConstraint(t1o, t2o, c.origins), failed)

          case (SPair(t1l, t1r), SPair(t2l, t2r)) =>
            unify(rest + TypeConstraint(t1l, t2l, c.origins) +
              TypeConstraint(t1r, t2r, c.origins), failed)

          case (SFunc(t1a, t1r), SFunc(t2a, t2r)) =>
            unify(rest + TypeConstraint(t1a, t2a, c.origins) +
              TypeConstraint(t1r, t2r, c.origins), failed)

          case (SChan(qs1, ts1), SChan(qs2, ts2)) =>
            if (ts1.size != ts2.size) unify(rest, failed + c) else {
              val ts1d: List[SType] =
                ts1 map (dequantify(Set.empty ++ (qs1 map (_._1)), _))
              val ts2d: List[SType] =
                ts2 map (dequantify(Set.empty ++ (qs2 map (_._1)), _))
              val constrs: List[Constraint] = (ts1d, ts2d).zipped
                .map(TypeConstraint(_, _, c.origins))
              unify(rest ++ constrs, failed)
            }

          case _ => unify(rest, failed + c)
        }
    }
  }

  def constraintsProc (
    p: Proc ,
    env: Map [ Name , SType ]
  ): ( SType , ConstraintSet ) = p match {
    case Send ( c , ts , ms , q ) => {
      val (tyC: SType, constrC: ConstraintSet) = constraintsExp (c, env)
      val (tyQ: SType, constrQ: ConstraintSet) = constraintsProc(q, env)
      val tyMs_constrMs: List[(SType, ConstraintSet)] =
        ms map (constraintsExp(_, env))
      val tyMs: List [ SType ] = tyMs_constrMs map (_._1)
      val constrMs: ConstraintSet =
        tyMs_constrMs.foldLeft(ConstraintSet.empty)(_ union _._2)
      val constrs: List [ Constraint ] = tyC match {
        case SChan ( qs , us ) =>
          if ( ms.size == us.size && ts.size == qs.size )
            ( tyMs , us map ( _ sTypeSubstFold (
              ( qs map ( _._1 ) ) zip ts ) ) , ms ).zipped
                .map { ( t1 , t2 , e ) =>
                  TypeConstraint ( t1 , t2 , List ( e ) )
                }
          else List (
            ArityConstraint ( ms.size , us.size , List ( p ) ) ,
            ArityConstraint ( ts.size , qs.size , List ( p ) ) )
        case _ =>
          List ( TypeConstraint ( tyC , SChan ( Nil , Nil ) , List ( c ) ) )
      }
      ( SProc , constrC union constrQ union constrMs ++ constrs )
    }
    case Receive ( _ , c , qs , as , q ) => {
      val (tyC: SType, constrC: ConstraintSet) = constraintsExp (c, env      )
      val (tyQ: SType, constrQ: ConstraintSet) = constraintsProc(q, env ++ as)
      val constr: Constraint = TypeConstraint ( tyC ,
        SChan ( qs map ( ( _ , List.empty ) ) , as map ( _._2 ) ) , List ( c ) )
      ( SProc , constrC union constrQ + constr )
    }
    case LetIn ( bind , ty , exp , p ) => {
      val (tyE: SType, constrE: ConstraintSet) = constraintsExp ( exp , env )
      val (tyP: SType, constrP: ConstraintSet) =
        constraintsProc ( p , env + ( bind -> tyE ) )
      ( SProc , constrE union constrP )
    }
    case IfThenElse ( exp , q1  , q2 ) => {
      val (tyE:  SType, constrE:  ConstraintSet) = constraintsExp (exp, env)
      val (tyQ1: SType, constrQ1: ConstraintSet) = constraintsProc(q1 , env)
      val (tyQ2: SType, constrQ2: ConstraintSet) = constraintsProc(q2 , env)
      ( SProc , constrE union constrQ1 union constrQ2
        + TypeConstraint ( tyE  , SBool , List ( exp ) )
        + TypeConstraint ( tyQ1 , tyQ2  , List ( p   ) ) )
    }
    case Parallel ( p , q ) => {
      val (tyP: SType, constrP: ConstraintSet) = constraintsProc(p, env)
      val (tyQ: SType, constrQ: ConstraintSet) = constraintsProc(q, env)
      ( SProc , constrP union constrQ )
    }
    case New ( bind , ty , p ) => constraintsProc ( p , env + ( bind -> ty ) )
    case End => ( SProc , ConstraintSet.empty )
  }

  def constraintsExp(e: Exp, env: Map[Name, SType]): (SType, ConstraintSet) =
    e match {
      case Variable   (name       ) => (env(name)         , ConstraintSet.empty)
      case IntLiteral (value      ) => (SInt              , ConstraintSet.empty)
      case BoolLiteral(value      ) => (SBool             , ConstraintSet.empty)
      case KharLiteral(value      ) => (SKhar             , ConstraintSet.empty)
      case ListExp    (Nil        ) =>
        (SList(SVar(fresh, List.empty)), ConstraintSet.empty)
      case ListExp    (exp :: exps) => {
        val (tyE:  SType, constrE:  ConstraintSet) = constraintsExp(exp, env)
        val (tyES: SType, constrES: ConstraintSet) =
          constraintsExp(ListExp(exps), env)
        (tyES, constrE union constrES
          + TypeConstraint(SList(tyE), tyES, List(e)))
      }
      case Pair(l, r) => {
        val (tyL: SType, constrL: ConstraintSet) = constraintsExp(l, env)
        val (tyR: SType, constrR: ConstraintSet) = constraintsExp(r, env)
        (SPair(tyL, tyR), constrL union constrR)
      }
      case UnExp(op, of) => {
        val (tyOf: SType, constrOf: ConstraintSet) = constraintsExp(of, env)
        val tyOp: SType = dequantify(Typecheck.typeOfUnOp(op))
        (tyOp.retTy, constrOf + TypeConstraint(tyOp.argTy, tyOf, List(e)))
      }
      case BinExp      ( op   , l  , r ) => {
        val (tyL: SType, constrL: ConstraintSet) = constraintsExp(l, env)
        val (tyR: SType, constrR: ConstraintSet) = constraintsExp(r, env)
        val tyOp: SType = dequantify(Typecheck.typeOfBinOp(op))
        (tyOp.retTy.retTy, constrL union constrR
          + TypeConstraint(tyOp.argTy      , tyL, List(e))
          + TypeConstraint(tyOp.retTy.argTy, tyR, List(e)))
      }
      case ChanLiteral ( StdOutName    ) =>
        (Typecheck.typeOfIOChan, ConstraintSet.empty)
      case ChanLiteral ( StdInName     ) =>
        (Typecheck.typeOfIOChan, ConstraintSet.empty)
      case ChanLiteral ( StdErrName    ) =>
        (Typecheck.typeOfIOChan, ConstraintSet.empty)
      case ChanLiteral ( _             ) =>
        throw new RuntimeException("ChanLiteral present in type-check")
    }

  /**
   * Replace quantified type variables with fresh type variables in an SType.
   */
  def dequantify(qs: Set[Name], ty: SType): SType =
    (qs foldLeft ty) { (t, q) => t sTypeSubst (q, SVar(fresh, List.empty)) }
  def dequantify(qs_ty: (Set[Name], SType)): SType =
    dequantify(qs_ty._1, qs_ty._2)
}

object Typecheck {

  def typeOfIOChan: SType = SChan(List(), List(SList(SKhar)))

  def typeOfBinOp(op: BinOp): (Set[Name], SType) = op match {
    case Add        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Sub        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Mul        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Div        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Mod        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Less       => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case LessEq     => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case Greater    => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case GreaterEq  => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case And        => (Set.empty   , SFunc(SBool, SFunc(SBool, SBool)))
    case Or         => (Set.empty   , SFunc(SBool, SFunc(SBool, SBool)))
    case Equal      => (Set(NumName(0)), SFunc(SVar(NumName(0), List.empty),
      SFunc(SVar(NumName(0), List.empty), SBool)))
    case NotEqual   => (Set(NumName(0)), SFunc(SVar(NumName(0), List.empty),
      SFunc(SVar(NumName(0), List.empty), SBool)))
    case Cons       => (Set(NumName(0)), SFunc(SVar(NumName(0), List.empty),
      SFunc(SList(SVar(NumName(0), List.empty)), SList(SVar(NumName(0),
        List.empty)))))
  }

  def typeOfUnOp(op: UnOp): (Set[Name], SType) = op match {
    case Not    => (Set.empty,
      SFunc(SBool, SBool))
    case PLeft  => (Set(NumName(0), NumName(1)), SFunc(SPair(SVar(NumName(0),
      List.empty), SVar(NumName(1), List.empty)), SVar(NumName(0), List.empty)))
    case PRight => (Set(NumName(0), NumName(1)), SFunc(SPair(SVar(NumName(0),
      List.empty), SVar(NumName(1), List.empty)), SVar(NumName(1), List.empty)))
    case Empty  => (Set(NumName(0)), SFunc(SList(SVar(NumName(0), List.empty)),
      SBool))
    case Head   => (Set(NumName(0)), SFunc(SList(SVar(NumName(0), List.empty)),
      SVar(NumName(0), List.empty)))
    case Tail   => (Set(NumName(0)), SFunc(SList(SVar(NumName(0), List.empty)),
      SList(SVar(NumName(0), List.empty))))
  }
}
