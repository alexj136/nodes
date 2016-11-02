package typecheck

import syntax._

sealed abstract class SType {

  /**
   * Given an SType assumed to be a function type (as would always be after
   * dequantification of a UnOp or BinOp), get the argument type of the
   * function.
   */
  def argTy: SType = this match {
    case SFunc(a, _) => a
    case _           => {
      val errMsg: String =
        "tried to get the argument type of a non-SFunc SType. SType.argTy() "  +
        "may only be called on STypes known to be SFunc, such as those "       +
        "returned from a call to dequantify()."
      throw new RuntimeException(errMsg)
    }
  }

  /**
   * Given an SType assumed to be a function type (as would always be after
   * dequantification of a UnOp or BinOp), get the return type of the function.
   */
  def retTy: SType = this match {
    case SFunc(_, r) => r
    case _           => {
      val errMsg: String =
        "tried to get the return type of a non-SFunc SType. SType.retTy() " +
        "may only be called on STypes known to be SFunc, such as those "    +
        "returned from a call to dequantify()."
      throw new RuntimeException(errMsg)
    }
  }

  /**
   * Substitute type variables in a type expression, of a given name, to another
   * given type expression. Only gives the correct result when n does not occur
   * in this. Only defined over types that do not contain quantifiers.
   */
  def sTypeSubst ( from: Name , to: SType ) : SType = this match {
    case SVar  ( n     ) if n == from => to
    case SVar  ( n     ) if n != from => this
    case SPair ( l , r )              =>
      SPair ( l sTypeSubst ( from , to ) , r sTypeSubst ( from , to ) )
    case SFunc ( a , r )              =>
      SFunc ( a sTypeSubst ( from , to ) , r sTypeSubst ( from , to ) )
    case SQuant ( _ , _ )             => throw new RuntimeException (
      "STypeSubst defined only over types that do not contain quantifiers." )
    case _                            => this
  }

  /**
   * Occurs check - check a type variable name does not occur within this type,
   * with which it must be unified. Such a situation would cause infinite
   * recursion during unification.
   */
  def hasOccurrenceOf ( n: Name ) : Boolean = ( n , this ) match {
    case ( x , SVar   ( y     ) ) => x == y
    case ( x , SChan  ( t     ) ) => ( t hasOccurrenceOf x )
    case ( x , SFunc  ( a , r ) ) =>
      ( a hasOccurrenceOf x ) || ( r hasOccurrenceOf x )
    case ( x , SPair  ( l , r ) ) =>
      ( l hasOccurrenceOf x ) || ( r hasOccurrenceOf x )
    case ( x , SQuant ( _ , _ ) ) => throw new RuntimeException (
      "SQuant not removed during occurrence check." )
    case _ => false
  }
}
case object SProc                          extends SType
case object SInt                           extends SType
case object SBool                          extends SType
case class  SChan  ( val t: SType        ) extends SType
case class  SPair  ( l: SType , r: SType ) extends SType
case class  SVar   ( n: Name             ) extends SType
case class  SQuant ( n: Name  , t: SType ) extends SType
case class  SFunc  ( a: SType , r: SType ) extends SType

case class ConstraintSet( val set: Set [ Constraint ] ) {
  def split: ( Option [ Constraint ] , ConstraintSet ) =
    set.toList match {
      case ( c :: cs ) => ( Some ( c ) , ConstraintSet( cs.toSet ) )
      case Nil         => ( None       , this                      )
    }
  def union ( other: ConstraintSet ) : ConstraintSet =
    ConstraintSet ( set union other.set )
  def + ( constr: Constraint ) : ConstraintSet =
    ConstraintSet ( set + constr )
  def map ( f: SType => SType ) : ConstraintSet =
    ConstraintSet( set map ( _ map f ) )
  override def toString: String = "Constraint set {\n" +
    ( set.toList.map ( "  " + _ + "\n" ).foldLeft ( "" ) ( _ + _ ) ) + "}\n"
}

case object ConstraintSet {
  def empty: ConstraintSet = ConstraintSet ( Set.empty )
}

case class Constraint
  ( val t1     : SType
  , val t2     : SType
  , val origin : SyntaxElement
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
    Constraint ( f ( t1 ) , f ( t2 ) , origin )

  def trivial: Boolean = t1 == t2

  override def toString: String =
    "Constraint " + t1 + " == " + t2 + " from " + origin.pos
}
  
object Typecheck {

  def checkProc(p: Proc): Option[SType] = {
    val (env, nn): (Map[Name, SType], Name) = initialEnv(p)
    val (tyP, constrP, nnP): (SType, ConstraintSet, Name) =
      constraintsProc(p, env, nn)
    val unifyFn: Option[SType => SType] = unify(constrP)
    if (unifyFn.isDefined)
      Some((unifyFn.get)(tyP))
    else
      None
  }

  /**
   * Constraint set unification
   */
  def unify(constrs: ConstraintSet): Option[SType => SType] = {
    val exception =
      new RuntimeException("SQuant not removed from type before unification")
    constrs.split match {
      case ( None , _          )              => Some ( identity )
      case ( Some ( c ) , rest ) if c.trivial => unify ( rest )
      case ( Some ( c ) , rest )              => c.asPair match {
        case ( SVar ( n ) , ty         ) if ! ( ty hasOccurrenceOf n ) =>
          unify ( rest map ( _ sTypeSubst ( n , ty ) ) )
            . map ( _ compose ( _ sTypeSubst ( n , ty ) ) )
        case ( ty         , SVar ( n ) ) if ! ( ty hasOccurrenceOf n ) =>
          unify ( rest map ( _ sTypeSubst ( n , ty ) ) )
            . map ( _ compose ( _ sTypeSubst ( n , ty ) ) )
        case ( SChan  ( t1m       ) , SChan  ( t2m       ) ) =>
          unify ( rest + Constraint ( t1m , t2m , c.origin ) )
        case ( SPair  ( t1l , t1r ) , SPair  ( t2l , t2r ) ) =>
          unify ( rest + Constraint ( t1l , t2l , c.origin ) +
            Constraint ( t1r , t2r , c.origin ) )
        case ( SFunc  ( t1a , t1r ) , SFunc  ( t2a , t2r ) ) =>
          unify ( rest + Constraint ( t1a , t2a , c.origin ) +
            Constraint ( t1r , t2r , c.origin ) )
        case ( SQuant ( _   , _   ) , _                    ) => throw exception
        case ( _                    , SQuant ( _   , _   ) ) => throw exception
        case _                                               => None
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
      val ( tyCh , constrCh , nnCh ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( ch , env , nn )
      val ( tyMsg , constrMsg , nnMsg ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( msg , env , nnCh )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env , nnMsg )
      ( SProc , constrCh union constrMsg union constrQ +
        Constraint ( tyCh , SChan ( tyMsg ) , p ) , nnQ )
    }
    case Receive    ( _    , ch  , bind , q ) => {
      val tyBind: SType = SVar ( nn )
      val ( tyCh , constrCh , nnCh ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( ch , env , nn.next )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env + ( bind -> tyBind ) , nnCh )
      ( SProc , constrCh union constrQ +
        Constraint ( tyCh , SChan ( tyBind ) , p ) , nnQ )
    }
    case LetIn      ( bind , exp , p        ) => {
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp ( exp , env , nn )
      val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( p , env + ( bind -> tyE ) , nnE )
      ( SProc , constrE union constrP , nnE )
    }
    case IfThenElse ( exp  , q1  , q2       ) => {
      val ( tyE , constrE , nnE ): ( SType , ConstraintSet , Name ) =
        constraintsExp( exp , env , nn )
      val ( tyQ1 , constrQ1 , nnQ1 ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q1 , env , nnE )
      val ( tyQ2 , constrQ2 , nnQ2 ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q2 , env , nnQ1 )
      ( SProc , constrE union constrQ1 union constrQ2
        + Constraint ( tyE , SBool , p )
        + Constraint ( tyQ1 , tyQ2 , p ) , nnQ2 )
    }
    case Parallel   ( p    , q              ) => {
      val ( tyP , constrP , nnP ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( p , env , nn )
      val ( tyQ , constrQ , nnQ ): ( SType , ConstraintSet , Name ) =
        constraintsProc ( q , env , nnP )
      ( SProc , constrP union constrQ , nnQ )
    }
    case New        ( bind , p              ) =>
      constraintsProc ( p , env + ( bind -> SChan ( SVar ( nn ) ) ) , nn.next )
    case End                                  =>
      ( SProc , ConstraintSet.empty , nn )
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
      (tyOp.retTy, constrOf + Constraint(tyOp.argTy, tyOf, e), nnOp)
    }
    case BinExp      ( op   , l  , r ) => {
      val (tyL, constrL, nnL): (SType, ConstraintSet, Name) =
        constraintsExp(l, env, nn)
      val (tyR, constrR, nnR): (SType, ConstraintSet, Name) =
        constraintsExp(r, env, nnL)
      val (tyOp, nnOp): (SType, Name) = dequantify(typeOfBinOp(op), nnR)
      (tyOp.retTy.retTy, constrL union constrR
        + Constraint(tyOp.argTy      , tyL, e)
        + Constraint(tyOp.retTy.argTy, tyR, e), nnOp)
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
    case SProc           => (SProc  , nn)
    case SInt            => (SInt   , nn)
    case SBool           => (SBool  , nn)
    case SChan ( t     ) => {
      val (deT, nnT): (SType, Name) = dequantify(t, nn)
      (SChan(deT), nnT)
    }
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
      case SChan ( t     ) => {
        val (subT, nnT): (SType, Name) = sTVarSubst(t, from, to, nn)
        (SChan(subT), nnT)
      }
      case SVar  ( n     ) => (SVar(if (n == from) to else n) , nn)
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
