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

case class Constraint
  ( val t1      : SType
  , val t2      : SType
  , val origins : List [ SyntaxElement ]
  ) {

  def asPair: ( SType , SType ) = ( t1 , t2 )

  override def equals ( other: Any ): Boolean =
    if ( other.isInstanceOf [ Constraint ] ) {
      val oc: Constraint = other.asInstanceOf[Constraint]
      ((this.t1 == oc.t1) && (this.t2 == oc.t2)) ||
      ((this.t2 == oc.t1) && (this.t1 == oc.t2))
    }
    else false

  def map ( f: SType => SType ): Constraint =
    Constraint ( f ( t1 ) , f ( t2 ) , origins )

  def trivial: Boolean = t1 == t2

  def pstr ( nameMap: Map [ Name , String ] ): String =
    s"Constraint $t1 == $t2 from:\n" +
      ( origins map ( e => s"${e.pstr(nameMap)}\n    at ${e.info}" ) ).toString

  def combineIfCompatible ( other: Constraint ): Constraint =
    if ( ! ( other equals this ) ) this else
      Constraint ( this.t1 , this.t2 , this.origins ++ other.origins )
}

object Typecheck {

  def checkProc ( p: Proc ): Option [ SType ] = {

    val nextName: Name = findNextName ( p.free )

    val ( tyP , constrP , nextNameP ): ( SType , ConstraintSet , Name ) =
      constraintsProc ( p , Map.empty , nextName )

    unify ( constrP , ConstraintSet.empty ) match {
      case Right ( unifyFn ) => Some ( unifyFn ( tyP ) )
      case Left  ( _       ) => None
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
      case ( None , _          )              =>
        if ( failed.isEmpty ) Right ( identity ) else Left ( failed )
      case ( Some ( c ) , rest ) if c.trivial => unify ( rest , failed )
      case ( Some ( c ) , rest )              => c.asPair match {

        case ( SVar ( n ) , ty         ) if ! ( ty free n ) =>
          val subFn: SType => SType = _ sTypeSubst ( n , ty )
          unify ( rest map subFn , failed map subFn )
            . right . map ( _ compose subFn )

        case ( ty         , SVar ( n ) ) if ! ( ty free n ) =>
          val subFn: SType => SType = _ sTypeSubst ( n , ty )
          unify ( rest map subFn , failed map subFn )
            . right . map ( _ compose subFn )

        case ( SChan  ( qs1 , ts1 ) , SChan  ( qs2 , ts2 ) ) => ???

        case ( SList  ( t1o       ) , SList  ( t2o       ) ) =>
          unify ( rest + Constraint ( t1o , t2o , c.origins ) , failed )

        case ( SPair  ( t1l , t1r ) , SPair  ( t2l , t2r ) ) =>
          unify ( rest + Constraint ( t1l , t2l , c.origins ) +
            Constraint ( t1r , t2r , c.origins ) , failed )

        case ( SFunc  ( t1a , t1r ) , SFunc  ( t2a , t2r ) ) =>
          unify ( rest + Constraint ( t1a , t2a , c.origins ) +
            Constraint ( t1r , t2r , c.origins ) , failed )

        case _ => unify ( rest , failed + c )
      }
    }
  }

  def constraintsProc(
    p: Proc,
    env: Map[Name, SType],
    nn: Name
  ): (SType, ConstraintSet, Name) = p match {
    case Send ( c , ts , ms , q ) => {
      val ( tyC , constrC , nnC ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( c , env , nn )
      tyC match {
        case SChan ( qs , us ) => {
          val usReplaced: List [ SType ] =
            us map ( _ sTypeSubstFold ( qs zip ts ) )
          ???
        }
        case _ => ???
      }
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env , nnC )
      ( SProc , constrC union constrQ + ??? , nnQ )
    }
    case Receive ( _ , c , qs , as , q ) => {
      val ( tyC , constrC , nnC ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( c , env , nn )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env ++ as , nnC )
      ( SProc , constrC union constrQ
        + Constraint ( tyC , SChan ( qs , as map ( _._2 ) ) , List ( c ) )
        , nnQ )
    }
    case LetIn ( bind , ty , exp , p ) => {
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( exp , env , nn )
      val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( p , env + ( bind -> tyE ) , nnE )
      ( SProc , constrE union constrP , nnP )
    }
    case IfThenElse ( exp , q1  , q2 ) => {
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp( exp , env , nn )
      val ( tyQ1 , constrQ1 , nnQ1 ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q1 , env , nnE )
      val ( tyQ2 , constrQ2 , nnQ2 ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q2 , env , nnQ1 )
      ( SProc , constrE union constrQ1 union constrQ2
        + Constraint ( tyE  , SBool , List ( exp ) )
        + Constraint ( tyQ1 , tyQ2  , List ( p   ) ) , nnQ2 )
    }
    case Parallel ( p , q ) => {
      val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( p , env , nn )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env , nnP )
      ( SProc , constrP union constrQ , nnQ )
    }
    case New ( bind , ty , p ) =>
      constraintsProc ( p , env + ( bind -> ty ) , nn )
    case End => ( SProc , ConstraintSet.empty , nn )
  }

  def constraintsExp(
    e: Exp,
    env: Map[Name, SType],
    nn: Name
  ): (SType, ConstraintSet, Name) = e match {
    case Variable    ( name          ) => (env(name), ConstraintSet.empty, nn)
    case IntLiteral  ( value         ) => (SInt     , ConstraintSet.empty, nn)
    case BoolLiteral ( value         ) => (SBool    , ConstraintSet.empty, nn)
    case ChanLiteral ( name          ) => (env(name), ConstraintSet.empty, nn)
    case KharLiteral ( value         ) => (SKhar    , ConstraintSet.empty, nn)
    case ListExp     ( Nil           ) =>
      (SList(SVar(nn)), ConstraintSet.empty, nn.next)
    case ListExp     ( exp :: exps   ) => {
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp( exp , env , nn )
      val ( tyES , constrES , nnES ): ( SType , ConstraintSet , Name ) =
        constraintsExp( ListExp ( exps ) , env , nnE )
      (tyES, constrE union constrES
        + Constraint ( SList ( tyE ) , tyES , List ( e ) ) , nnES )
    }
    case Pair        ( l    , r      ) => {
      val (tyL, constrL, nnL): (SType, ConstraintSet, Name) =
        constraintsExp(l, env, nn)
      val (tyR, constrR, nnR): (SType, ConstraintSet, Name) =
        constraintsExp(r, env, nnL)
      (SPair(tyL, tyR), constrL union constrR, nnR)
    }
    case UnExp       ( op   , of     ) => {
      val (tyOf, constrOf, nnOf): (SType, ConstraintSet, Name) =
        constraintsExp(of, env, nn)
      val (tyOp, nnOp): (SType, Name) = dequantify(typeOfUnOp(op), nnOf)
      (tyOp.retTy, constrOf + Constraint(tyOp.argTy, tyOf, List(e)), nnOp)
    }
    case BinExp      ( op   , l  , r ) => {
      val (tyL, constrL, nnL): (SType, ConstraintSet, Name) =
        constraintsExp(l, env, nn)
      val (tyR, constrR, nnR): (SType, ConstraintSet, Name) =
        constraintsExp(r, env, nnL)
      val (tyOp, nnOp): (SType, Name) = dequantify(typeOfBinOp(op), nnR)
      (tyOp.retTy.retTy, constrL union constrR
        + Constraint(tyOp.argTy      , tyL, List(e))
        + Constraint(tyOp.retTy.argTy, tyR, List(e)), nnOp)
    }
  }

  def typeOfBinOp(op: BinOp): (Set[Name], SType) = op match {
    case Add        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Sub        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Mul        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Div        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Mod        => (Set.empty   , SFunc(SInt , SFunc(SInt , SInt )))
    case Equal      => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case NotEqual   => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case Less       => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case LessEq     => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case Greater    => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case GreaterEq  => (Set.empty   , SFunc(SInt , SFunc(SInt , SBool)))
    case And        => (Set.empty   , SFunc(SBool, SFunc(SBool, SBool)))
    case Or         => (Set.empty   , SFunc(SBool, SFunc(SBool, SBool)))
    case Cons       => (Set(Name(0)), SFunc(SVar(Name(0)),
      SFunc(SList(SVar(Name(0))),SList(SVar(Name(0))))))
  }

  def typeOfUnOp(op: UnOp): (Set[Name], SType) = op match {
    case Not    => (Set.empty,
      SFunc(SBool, SBool))
    case PLeft  => (Set(Name(0), Name(1)),
      SFunc(SPair(SVar(Name(0)), SVar(Name(1))), SVar(Name(0))))
    case PRight => (Set(Name(0), Name(1)),
      SFunc(SPair(SVar(Name(0)), SVar(Name(1))), SVar(Name(1))))
    case Empty  => (Set(Name(0)),
      SFunc(SList(SVar(Name(0))), SBool))
    case Head   => (Set(Name(0)),
      SFunc(SList(SVar(Name(0))), SVar(Name(0))))
    case Tail   => (Set(Name(0)),
      SFunc(SList(SVar(Name(0))), SList(SVar(Name(0)))))
  }

  /**
   * Replace quantified type variables with fresh type variables in an SType.
   * The Name parameter & Name return value is the next globally available Name,
   * before and after the dequantification.
   */
  def dequantify(qs_ty: (Set[Name], SType), nn: Name): (SType, Name) =
    (qs_ty._1 foldLeft (qs_ty._2, nn)) { case ((t, n), q) =>
      (t.sTypeSubst(q, SVar(n)), n.next)
    }
}
