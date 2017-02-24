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

  def checkProc(p: Proc): Option[SType] = {
    val (env, nn): (Map[Name, SType], Name) = initialEnv(p)
    val (tyP, constrP, nnP): (SType, ConstraintSet, Name) =
      constraintsProc(p, env, nn)
    unify(constrP, ConstraintSet.empty) match {
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
      case ( None , _          )              =>
        if ( failed.isEmpty ) Right ( identity ) else Left ( failed )
      case ( Some ( c ) , rest ) if c.trivial => unify ( rest , failed )
      case ( Some ( c ) , rest )              => c.asPair match {

        case ( SVar ( n ) , ty         ) if ! ( ty hasOccurrenceOf n ) =>
          val subFn: SType => SType = _ sTypeSubst ( n , ty )
          unify ( rest map subFn , failed map subFn )
            . right . map ( _ compose subFn )

        case ( ty         , SVar ( n ) ) if ! ( ty hasOccurrenceOf n ) =>
          val subFn: SType => SType = _ sTypeSubst ( n , ty )
          unify ( rest map subFn , failed map subFn )
            . right . map ( _ compose subFn )

        case ( SChan  ( t1m       ) , SChan  ( t2m       ) ) =>
          unify ( rest + Constraint ( t1m , t2m , c.origins ) , failed )

        case ( SList  ( t1o       ) , SList  ( t2o       ) ) =>
          unify ( rest + Constraint ( t1o , t2o , c.origins ) , failed )

        case ( SPair  ( t1l , t1r ) , SPair  ( t2l , t2r ) ) =>
          unify ( rest + Constraint ( t1l , t2l , c.origins ) +
            Constraint ( t1r , t2r , c.origins ) , failed )

        case ( SFunc  ( t1a , t1r ) , SFunc  ( t2a , t2r ) ) =>
          unify ( rest + Constraint ( t1a , t2a , c.origins ) +
            Constraint ( t1r , t2r , c.origins ) , failed )

        case ( SQuant ( _   , _   ) , _                    ) => throw exception
        case ( _                    , SQuant ( _   , _   ) ) => throw exception
        case _                                               =>
          unify ( rest , failed + c )
      }
    }
  }

  def initialEnv(p: Proc): (Map[Name, SType], Name) = {
    var map: Map[Name, SType] = Map.empty
    var name: Name = new Name(0)
    p.chanLiterals.foreach { c =>
      map = map + (c -> SChan(SVar(name)))
      name = name.next
    }
    (map, name)
  }

  def constraintsProc(
    p: Proc,
    env: Map[Name, SType],
    nn: Name
  ): (SType, ConstraintSet, Name) = p match {
    case Send       ( ch   , msg , q        ) => {
      val newVar: SType = SVar ( nn )
      val ( tyCh , constrCh , nnCh ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( ch , env , nn.next )
      val ( tyMsg , constrMsg , nnMsg ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( msg , env , nnCh )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env , nnMsg )
      ( SProc , constrCh union constrMsg union constrQ
        + Constraint ( tyCh  , SChan ( newVar ) , List ( ch  ) )
        + Constraint ( tyMsg , newVar           , List ( msg ) ) , nnQ.next )
    }
    case Receive    ( _    , ch  , bind , q ) => {
      // TODO let-poly (see LetIn code)
      val tyBind: SType = SVar ( nn )
      val ( tyCh , constrCh , nnCh ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( ch , env , nn.next )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env + ( bind -> tyBind ) , nnCh )
      ( SProc , constrCh union constrQ
        + Constraint ( tyCh , SChan ( tyBind ) , List ( p ) ) , nnQ )
    }
    case LetIn      ( bind , exp , p        ) => {
      // Uses the efficient let-polymorphism typing rules described on pages 333
      // and 334 of Ben Pierce's Types and Programming Languages.
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( exp , env , nn )
      unify ( constrE , ConstraintSet.empty ) match {
        case Left  ( unsatisfiableConstraints ) =>
          val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
            constraintsProc ( p , env + ( bind -> tyE ) , nnE )
          ( SProc , unsatisfiableConstraints union constrP , nnP )
        case Right ( ePrincipalSubst          ) => {
          val principalTyE: SType = ePrincipalSubst ( tyE )
          val envWithESub: Map[Name, SType] = env mapValues ePrincipalSubst
          val generalisableVars: Set[Name] = principalTyE.free --
            ( ( ( envWithESub.values ) map ( _.free ) )
              .fold ( Set.empty ) ( _ union _ ) )
          val generalisedPrincTyE: SType =
            generalisableVars.foldRight ( principalTyE ) ( SQuant ( _ , _ ) )
          val newEnv: Map[Name, SType] =
            envWithESub + (bind -> generalisedPrincTyE)
          val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
            constraintsProc ( p , newEnv , nnE )
          ( SProc , constrE union constrP , nnP )
        }
      }
    }
    case IfThenElse ( exp  , q1  , q2       ) => {
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp( exp , env , nn )
      val ( tyQ1 , constrQ1 , nnQ1 ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q1 , env , nnE )
      val ( tyQ2 , constrQ2 , nnQ2 ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q2 , env , nnQ1 )
      ( SProc , constrE union constrQ1 union constrQ2
        + Constraint ( tyE , SBool , List ( exp ) )
        + Constraint ( tyQ1 , tyQ2 , List ( p   ) ) , nnQ2 )
    }
    case Parallel   ( p    , q              ) => {
      val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( p , env , nn )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env , nnP )
      ( SProc , constrP union constrQ , nnQ )
    }
    case New        ( bind , p              ) =>
      // TODO let-poly (see LetIn code)
      constraintsProc ( p , env + ( bind -> SChan ( SVar ( nn ) ) ) , nn.next )
    case End                                  =>
      ( SProc , ConstraintSet.empty , nn )
  }

  def constraintsExp(
    e: Exp,
    env: Map[Name, SType],
    nn: Name
  ): (SType, ConstraintSet, Name) = e match {
    case Variable    ( name          ) => {
      val (deqTyName, nnName): (SType, Name) = dequantify(env(name), nn)
      (deqTyName, ConstraintSet.empty, nnName)
    }
    case IntLiteral  ( value         ) => (SInt     , ConstraintSet.empty, nn)
    case BoolLiteral ( value         ) => (SBool    , ConstraintSet.empty, nn)
    case ChanLiteral ( name          ) => {
      val (deqTyName, nnName): (SType, Name) = dequantify(env(name), nn)
      (deqTyName, ConstraintSet.empty, nnName)
    }
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
    case Cons       => SQuant(new Name(0),
      SFunc(SVar(new Name(0)),
      SFunc(SList(SVar(new Name(0))),SList(SVar(new Name(0))))))
  }

  def typeOfUnOp(op: UnOp): SType = op match {
    case Not    => SFunc(SBool, SBool)
    case PLeft  => SQuant(new Name(0), SQuant(new Name(1),
      SFunc(SPair(SVar(new Name(0)), SVar(new Name(1))), SVar(new Name(0)))))
    case PRight => SQuant(new Name(0), SQuant(new Name(1),
      SFunc(SPair(SVar(new Name(0)), SVar(new Name(1))), SVar(new Name(1)))))
    case Empty  => SQuant(new Name(0), SFunc(SList(SVar(new Name(0))), SBool))
    case Head   => SQuant(new Name(0),
      SFunc(SList(SVar(new Name(0))), SVar(new Name(0))))
    case Tail   => SQuant(new Name(0),
      SFunc(SList(SVar(new Name(0))), SList(SVar(new Name(0)))))
  }

  /**
   * Remove quantifiers from an SType. The Name parameter & Name return value is
   * the next globally available Name, before and after the dequantification.
   */
  def dequantify(ty: SType, nn: Name): (SType, Name) = ty match {
    case SProc           => (SProc  , nn)
    case SInt            => (SInt   , nn)
    case SBool           => (SBool  , nn)
    case SKhar           => (SKhar  , nn)
    case SChan ( t     ) => {
      val (deT, nnT): (SType, Name) = dequantify(t, nn)
      (SChan(deT), nnT)
    }
    case SVar  ( n     ) => (SVar(n), nn)
    case SList ( t     ) => {
      val (deT, nnT): (SType, Name) = dequantify(t, nn)
      (SList(deT), nnT)
    }
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
      val (newT, nnT): (SType, Name) = sTVarSubst(t, n, nn, nn.next)
      dequantify(newT, nnT)
    }
  }

  /**
   * Substitute type variables in a type expression, of a given name, to another
   * given name. Alpha conversion of quantified expressions is sometimes
   * necessary to prevent erroneous capture, so a next-name parameter is
   * required and returned.
   */
  def sTVarSubst(ty: SType, from: Name, to: Name, nn: Name): (SType, Name) =
    ty match {
      case SProc           => (SProc                          , nn)
      case SInt            => (SInt                           , nn)
      case SBool           => (SBool                          , nn)
      case SKhar           => (SKhar                          , nn)
      case SChan ( t     ) => {
        val (subT, nnT): (SType, Name) = sTVarSubst(t, from, to, nn)
        (SChan(subT), nnT)
      }
      case SVar  ( n     ) => (SVar(if (n == from) to else n) , nn)
      case SList ( t     ) => {
        val (subT, nnT): (SType, Name) = sTVarSubst(t, from, to, nn)
        (SList(subT), nnT)
      }
      case SPair ( l , r ) => {
        val (subL, nnL): (SType, Name) = sTVarSubst(l, from, to, nn)
        val (subR, nnR): (SType, Name) = sTVarSubst(r, from, to, nnL)
        (SPair(subL, subR), nnR)
      }
      case SFunc ( a , r ) => {
        val (subA, nnA): (SType, Name) = sTVarSubst(a, from, to, nn)
        val (subR, nnR): (SType, Name) = sTVarSubst(r, from, to, nnA)
        (SFunc(subA, subR), nnR)
      }
      case SQuant( n , t ) =>
        if (n == from) (SQuant(n, t), nn)
        // If substituting within the quantifier would cause this quantifier to
        // erroneously capture the substituted name, first rename the quantifier
        // and its bound variables to a new name.
        else if (n == to) {
          val (subT, nnT): (SType, Name) = sTVarSubst(t, n, nn, nn.next)
          val (subSubT, nnST): (SType, Name) = sTVarSubst(subT, from, to, nnT)
          (SQuant(nn, subSubT), nnST)
        }
        else {
          val (subT, nnT): (SType, Name) = sTVarSubst(t, from, to, nn)
          (SQuant(n, subT), nnT)
        }
    }
}
