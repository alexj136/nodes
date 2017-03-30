package syntax
import scala.util.parsing.input.Positional

sealed trait SyntaxElement {
  var info: Info = NoInfo
  def setInfo(i: Info): Unit = this.info = i
  def pstr(names: Map[Name, String]): String
  def free: Set[Name]
}

abstract class Name extends SyntaxElement

case class NumName(val id: Int) extends Name {
  def next: NumName = new NumName(this.id + 1)
  def pstr(names: Map[Name, String]): String =
    names getOrElse (this, this.toString)
  def free: Set[Name] = Set(this)
  override def toString: String = s"<${this.id}>" // for debugging
}

abstract class PreDefName extends Name {
  def pstr(names: Map[Name, String]): String = this.toString
  def free: Set[Name] = Set(this)
}

case object StdOutName extends PreDefName {
  override def toString: String = "stdout" 
}

case object StdInName  extends PreDefName {
  override def toString: String = "stdin"
}

case object StdErrName extends PreDefName {
  override def toString: String = "stderr"
}

object findNextName extends Function1[Set[Name], NumName] {
  def apply(names: Set[Name]): NumName =
      if ((names filter (_.isInstanceOf[NumName])).isEmpty) NumName ( 0 ) else
    NumName ( ( names.map ( _.asInstanceOf[NumName].id ).max ) + 1 )
}

abstract class Info {
  override def toString: String = this match {
    case NoInfo                                   => "<no data>"
    case SrcPosInfo ( ( ll , lc ) , ( rl , rc ) ) =>
      s"source position spanning line $ll, column $lc to line $rl, column $rc"
  }
}
case class SrcPosInfo(val lPos: (Int, Int), val rPos: (Int, Int)) extends Info
case object NoInfo extends Info

sealed abstract class Proc extends SyntaxElement {

  def pstr(names: Map[Name, String]): String = this match {
    case Send       ( c     , ts  , ms   , p      ) =>
      "send " + (c pstr names) + "; " +
      ((ts map (_ pstr names)) mkString ", ") +
      "; " + ((ms map (_ pstr names)) mkString ", ") + ". " + (p pstr names)
    case Receive    ( r     , c   , qs   , as , p ) =>
      (if (r) "server" else "receive") + " " + (c pstr names) + "; " +
      ((qs map (_ pstr names)) mkString ", ") + "; " + ((as map {
        case (n, t) => s"${n pstr names}: ${t pstr names}"
      }) mkString ", ") + ". " + (p pstr names)
    case LetIn      ( bind  , t , exp , p         ) =>
      s"let ${bind pstr names}: ${t pstr names} = " +
      s"${exp pstr names} . ${p pstr names}"
    case IfThenElse ( exp   , tP  , fP            ) =>
      s"if ${exp pstr names} then ${tP pstr names} else ${fP pstr names} endif"
    case Parallel   ( p     , q                   ) =>
      s"[ ${p pstr names} | ${q pstr names} ]"
    case New        ( name  , t , p               ) =>
      s"new ${name pstr names}: ${t pstr names} . ${p pstr names}"
    case End                                       => "end"
  }

  def chanLiterals: Set[Name] = this match {
    case Send       ( c , _ , ms , p     ) =>
      c.chanLiterals union p.chanLiterals union
        ( ms map ( _.chanLiterals ) ).fold ( Set.empty ) ( _ union _ )
    case Receive    ( _ , e , _  , _ , p ) =>
      e.chanLiterals union p.chanLiterals
    case LetIn      ( _ , _ , e  , p     ) =>
      e.chanLiterals union p.chanLiterals
    case IfThenElse ( e , p , q          ) =>
      e.chanLiterals union p.chanLiterals union q.chanLiterals
    case Parallel   ( p , q              ) =>
      p.chanLiterals union q.chanLiterals
    case New        ( _ , _ , p          ) =>
      p.chanLiterals
    case End                               =>
      Set.empty
  }

  def free: Set[Name] = this match {
    case IfThenElse ( e , p  , q           ) => e.free union p.free union q.free
    case Parallel   ( p , q                ) => p.free union q.free
    case New        ( n , t  , p           ) => (p.free - n) union t.free
    case End                                 => Set.empty
    case LetIn      ( n , t  , e  , p      ) =>
      e.free union (p.free - n) union t.free
    case Send       ( c , ts , ms , p      ) => c.free union p.free union
      ( ( ms map ( _.free ) ) ++ ( ts map ( _.free ) ) )
        .fold ( Set.empty ) ( _ union _ )
    case Receive    ( _ , c  , qs , as , p ) => {
      val freeInP: Set [ Name ] = p.free -- ( as map ( _._1 ) )
      val freeInAs: Set [ Name ] = ( ( ( as map ( _._2 ) ).map ( _.free ) )
        .fold ( Set.empty ) ( _ union _ ) ) -- qs
      c.free union freeInP union freeInAs
    }
  }

  /** Decompose top-level parallel compositions into a list of processes.
   */
  def listify: List[Proc] = this match {
    case Parallel ( p , q ) => p.listify ++ q.listify
    case End                => Nil
    case _                  => List(this)
  }

  /** Remove top-level servers from a process (for convenient result printing)
   */
  def noServers: Proc = Proc fromList (this.listify filter {
    case Receive(true, _, _, _, _) => false
    case _                         => true
  })

  /** Alpha-equivalence for processes. If the processes are alpha-equivalent, a
   *  Some of a map containing the 'name equlivalences' between this and that
   *  are returned in an Option. If they are not alpha equivalent, None is
   *  returned.
   */
  def alphaEquiv(that: Proc): Option[Map[Name, Name]] = (this, that) match {
    case ( Send      (c, ts, ms, p      ) , Send      (d, us, ns, q    ) ) =>
      if (ts.size != us.size || ms.size != ns.size) None else
        alphaEquivCombineMany(List(
          c alphaEquiv d,
          p alphaEquiv q,
          alphaEquivCombineMany((ts, us).zipped map ( _ alphaEquiv _ )),
          alphaEquivCombineMany((ms, ns).zipped map ( _ alphaEquiv _ ))
        ))
    case ( Receive   (r, c , qs, as, p  ) , Receive   (s, d , rs, bs, q) ) =>
      if (r != s || qs.size != rs.size || as.size != bs.size) None else
        alphaEquivCombineMany(List(
          c alphaEquiv d,
          alphaEquivEnsureManyBindings(
            alphaEquivCombineMany(
              (as map (_._2), bs map (_._2)).zipped map ( _ alphaEquiv _ )),
            qs zip rs),
          alphaEquivEnsureManyBindings(
            p alphaEquiv q, (as map (_._1)) zip (bs map (_._1)))
        ))
    case ( LetIn     (n, t , x , p      ) , LetIn     (m, u , y , q    ) ) =>
      alphaEquivCombineMany(List(
        x alphaEquiv y,
        t alphaEquiv u,
        alphaEquivEnsureBinding(p alphaEquiv q, n, m)
      ))
    case ( IfThenElse(a, p , r          ) , IfThenElse(b, q , s        ) ) =>
      alphaEquivCombineMany(List(
        a alphaEquiv b,
        p alphaEquiv q,
        r alphaEquiv s
      ))
    case ( Parallel  (p, r              ) , Parallel  (q, s            ) ) =>
      alphaEquivCombine(p alphaEquiv q, r alphaEquiv s)
    case ( New       (n, t, p           ) , New       (m, u, q         ) ) =>
      alphaEquivCombine(t alphaEquiv u,
        alphaEquivEnsureBinding(p alphaEquiv q, n, m))
    case ( End                            , End                          ) =>
      Some(Map.empty)
    case ( _                              , _                            ) =>
      None
  }
}

case class Send
  ( chan  : Exp
  , types : List [ SType ]
  , msgs  : List [ Exp   ]
  , p     : Proc
  ) extends Proc

case class Receive
  ( repl   : Boolean
  , chan   : Exp
  , tyArgs : List [ Name ]
  , args   : List [ ( Name , SType ) ]
  , p      : Proc
  ) extends Proc

case class LetIn
  ( name : Name
  , ty   : SType
  , exp  : Exp
  , p    : Proc
  ) extends Proc

case class IfThenElse
  ( cond   : Exp
  , trueP  : Proc
  , falseP : Proc
  ) extends Proc

case class Parallel
  ( p : Proc
  , q : Proc
  ) extends Proc

case class New
  ( name : Name
  , ty   : SType
  , p    : Proc
  ) extends Proc

case object End extends Proc

object Proc {
  def fromList(ps: List[Proc]): Proc = ps match {
    case      Nil => End
    case p :: Nil => p
    case p :: qs  => Parallel(p, fromList(qs))
  }
}

sealed abstract class Exp extends SyntaxElement {

  def chanLiterals: Set[Name] = this match {
    case Variable    ( _         ) => Set.empty
    case IntLiteral  ( _         ) => Set.empty
    case BoolLiteral ( _         ) => Set.empty
    case KharLiteral ( _         ) => Set.empty
    case ChanLiteral ( n         ) => Set(n)
    case Pair        ( l , r     ) => l.chanLiterals union r.chanLiterals
    case UnExp       ( _ , e     ) => e.chanLiterals
    case BinExp      ( _ , l , r ) => l.chanLiterals union r.chanLiterals
    case ListExp     ( Nil       ) => Set.empty
    case ListExp     ( e :: es   ) =>
      e.chanLiterals union (ListExp(es).chanLiterals)
  }

  def free: Set[Name] = this match {
    case Variable    ( n         ) => Set(n)
    case IntLiteral  ( _         ) => Set.empty
    case BoolLiteral ( _         ) => Set.empty
    case ChanLiteral ( _         ) => Set.empty
    case KharLiteral ( _         ) => Set.empty
    case Pair        ( l , r     ) => l.free union r.free
    case UnExp       ( _ , e     ) => e.free
    case BinExp      ( _ , l , r ) => l.free union r.free
    case ListExp     ( Nil       ) => Set.empty
    case ListExp     ( e :: es   ) => e.free union (ListExp(es).free)
  }

  def contains(n: Name): Boolean = this match {
    case Variable    ( m         ) => n == m
    case IntLiteral  ( _         ) => false
    case BoolLiteral ( _         ) => false
    case ChanLiteral ( _         ) => false
    case KharLiteral ( _         ) => false
    case Pair        ( l , r     ) => (l contains n) || (r contains n)
    case UnExp       ( _ , e     ) => e contains n
    case BinExp      ( _ , l , r ) => (l contains n) || (r contains n)
    case ListExp     ( es        ) => (es map (_ contains n)) exists identity
  }

  def pstr(names: Map[Name, String]): String = this match {
    case Variable    ( name          ) =>
      name pstr names
    case IntLiteral  ( value         ) =>
      value.toString
    case BoolLiteral ( value         ) =>
      value.toString
    case ChanLiteral ( name          ) =>
      name pstr names
    case KharLiteral ( value         ) =>
      s"'$value'"
    case Pair        ( l    , r      ) =>
      s"{ ${l pstr names} , ${r pstr names} }"
    case UnExp       ( ty   , of     ) =>
      s"( ${ty.toString} ${of pstr names} )"
    case BinExp      ( ty   , l  , r ) =>
      s"( ${l pstr names} ${ty.toString} ${r pstr names} )"
    case ListExp     ( es            )
      if es.forall { case KharLiteral(_) => true case _ => false } =>
        (es map (_.asInstanceOf[KharLiteral].value.toString))
        .foldLeft("")(_ ++ _)
    case ListExp     ( es            ) =>
      (es map (_ pstr names)).mkString("[ ", ",", " ]")
  }

  /** Alpha-equivalence for expressions
   */
  def alphaEquiv(that: Exp): Option[Map[Name, Name]] = {

    (this, that) match {
      case ( Variable    ( n         ) , Variable    ( m         ) ) =>
        Some(Map(n -> m))
      case ( IntLiteral  ( a         ) , IntLiteral  ( b         ) ) =>
        if (a == b) Some(Map.empty) else None
      case ( BoolLiteral ( a         ) , BoolLiteral ( b         ) ) =>
        if (a == b) Some(Map.empty) else None
      case ( ChanLiteral ( n         ) , ChanLiteral ( m         ) ) =>
        Some(Map(n -> m))
      case ( KharLiteral ( a         ) , KharLiteral ( b         ) ) =>
        if (a == b) Some(Map.empty) else None
      case ( Pair        ( a , c     ) , Pair        ( b , d     ) ) =>
        alphaEquivCombine(a alphaEquiv b, c alphaEquiv d)
      case ( UnExp       ( a , c     ) , UnExp       ( b , d     ) ) =>
        if (a == b) c alphaEquiv d else None
      case ( BinExp      ( a , c , e ) , BinExp      ( b , d , f ) ) =>
        if (a == b) alphaEquivCombine(c alphaEquiv d, e alphaEquiv f) else None
      case ( ListExp     ( Nil       ) , ListExp     ( Nil       ) ) =>
        Some(Map.empty)
      case ( ListExp     ( e :: es   ) , ListExp     ( f :: fs   ) ) =>
        alphaEquivCombine(e alphaEquiv f, ListExp(es) alphaEquiv ListExp(fs))
      case ( _                         , _                         ) => None
    }
  }
}

case class Variable    ( name:      Name                          ) extends Exp
case class IntLiteral  ( value:     Int                           ) extends Exp
case class BoolLiteral ( value:     Boolean                       ) extends Exp
case class ChanLiteral ( name:      Name                          ) extends Exp
case class KharLiteral ( value:     Char                          ) extends Exp
case class Pair        ( lhs:       Exp     , rhs: Exp            ) extends Exp
case class UnExp       ( unOpType:  UnOp    , of:  Exp            ) extends Exp
case class BinExp      ( binOpType: BinOp   , lhs: Exp , rhs: Exp ) extends Exp
case class ListExp     ( exps:      List[Exp]                     ) extends Exp

sealed abstract class BinOp extends SyntaxElement {

  def pstr(names: Map[Name, String]): String = this.toString
  def free: Set[Name] = Set.empty
  def alphaEquiv(that: BinOp): Option[Map[Name, Name]] =
    if (this == that) Some(Map.empty) else None

  override def toString: String = this match {
    case Add        => "+"
    case Sub        => "-"
    case Mul        => "*"
    case Div        => "/"
    case Mod        => "%"
    case Equal      => "=="
    case NotEqual   => "!="
    case Less       => "<"
    case LessEq     => "<="
    case Greater    => ">"
    case GreaterEq  => ">="
    case And        => "&&"
    case Or         => "||"
    case Cons       => "::"
  }
}
case object Add       extends BinOp
case object Sub       extends BinOp
case object Mul       extends BinOp
case object Div       extends BinOp
case object Mod       extends BinOp
case object Equal     extends BinOp
case object NotEqual  extends BinOp
case object Less      extends BinOp
case object LessEq    extends BinOp
case object Greater   extends BinOp
case object GreaterEq extends BinOp
case object And       extends BinOp
case object Or        extends BinOp
case object Cons      extends BinOp

sealed abstract class UnOp extends SyntaxElement {

  def pstr(names: Map[Name, String]): String = this.toString
  def free: Set[Name] = Set.empty
  def alphaEquiv(that: UnOp): Option[Map[Name, Name]] =
    if (this == that) Some(Map.empty) else None

  override def toString: String = this match {
    case Not    => "!"
    case PLeft  => "<-"
    case PRight => "->"
    case Empty  => "?"
    case Head   => "*--"
    case Tail   => "-**"
  }
}
case object Not    extends UnOp
case object PLeft  extends UnOp
case object PRight extends UnOp
case object Empty  extends UnOp
case object Head   extends UnOp
case object Tail   extends UnOp

sealed abstract class SType extends SyntaxElement {

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
   * given type expression. Only terminates when the name 'from' does not occur
   * in this, so run an occurs-check first.
   */
  def sTypeSubst ( from: Name , to: SType ) : SType = this match {
    case SVar   ( n       ) => if ( n == from ) to else this
    case SList  ( t       ) => SList ( t sTypeSubst ( from , to ) )
    case SPair  ( l  , r  ) =>
      SPair ( l sTypeSubst ( from , to ) , r sTypeSubst ( from , to ) )
    case SFunc  ( a  , r  ) =>
      SFunc ( a sTypeSubst ( from , to ) , r sTypeSubst ( from , to ) )
    case SChan  ( qs , ts ) =>
      if ( ( qs filter ( to.free ( _ ) ) ).size > 0 )
        SChan ( qs , ts map ( _ sTypeSubst ( from , to ) ) )
      else {
        val fresh: NumName = findNextName ( ( ( ts map ( _.free ) )
          .fold ( Set.empty ) ( _ union _ ) ) union to.free ++ qs + from )
        val allFresh: List[ NumName ] = List.empty ++
          ( ( fresh.id until ( fresh.id + qs.size ) ) map ( NumName ( _ ) ) )
        SChan ( allFresh ,
          ts.map ( _ sTypeSubstFold ( qs zip ( allFresh map ( SVar ( _ ) ) ) ) )
            .map ( _ sTypeSubst ( from , to ) ) )
      }
    case _                                      => this
  }

  def sTypeSubstFold ( fromTos: List [ ( Name , SType ) ] ) : SType =
    fromTos match {
      case Nil               => this
      case ( n , t ) :: rest =>
        ( this sTypeSubst ( n , t ) ) sTypeSubstFold rest
    }

  override def pstr(names: Map[Name, String]): String = this match {
    case SProc              => "process"
    case SInt               => "int"
    case SBool              => "bool"
    case SKhar              => "char"
    case SChan  ( qs , ts ) => "@{" + ((qs map (_ pstr names)) mkString ", ") +
      "; " + ((ts map (_ pstr names)) mkString ", ") + "}"
    case SList  ( t       ) => s"[${t pstr names}]"
    case SPair  ( l  , r  ) => s"( ${l pstr names} , ${r pstr names} )"
    case SVar   ( n       ) => n pstr names
    case SFunc  ( a  , r  ) => s"(${a pstr names} => ${r pstr names})"
  }

  override def free: Set[Name] = this match {
    case SProc              => Set.empty
    case SInt               => Set.empty
    case SBool              => Set.empty
    case SKhar              => Set.empty
    case SChan  ( qs , ts ) =>
      ((ts map (_.free)).fold(Set.empty)(_ union _)) -- qs
    case SList  ( t       ) => t.free
    case SPair  ( l  , r  ) => l.free union r.free
    case SVar   ( n       ) => Set(n)
    case SFunc  ( a  , r  ) => a.free union r.free
  }

  def alphaEquiv(that: SType): Option[Map[Name, Name]] = (this, that) match {
      case (SProc             , SProc             ) => Some(Map.empty)
      case (SInt              , SInt              ) => Some(Map.empty)
      case (SBool             , SBool             ) => Some(Map.empty)
      case (SKhar             , SKhar             ) => Some(Map.empty)
      case (SChan  ( qs , ts ), SChan  ( rs , us )) => {
        if (qs.size != rs.size || ts.size != us.size) None else
          alphaEquivEnsureManyBindings(alphaEquivCombineMany(
            (ts, us).zipped map (_ alphaEquiv _)), qs zip rs)
      }
      case (SList  ( t       ), SList  ( u       )) => t alphaEquiv u
      case (SPair  ( l1 , r1 ), SPair  ( l2 , r2 )) =>
        alphaEquivCombine(l1 alphaEquiv l2, r1 alphaEquiv r2)
      case (SVar   ( n       ), SVar   ( m       )) => Some(Map(n -> m))
      case (SFunc  ( a1 , r1 ), SFunc  ( a2 , r2 )) =>
        alphaEquivCombine(a1 alphaEquiv a2, r1 alphaEquiv r2)
      case (_                 , _                 ) => None
  }
}
case object SProc                                      extends SType
case object SInt                                       extends SType
case object SBool                                      extends SType
case object SKhar                                      extends SType
case class  SChan  ( qs: List[Name], ts: List[SType] ) extends SType
case class  SList  ( t:  SType                       ) extends SType
case class  SPair  ( l:  SType     , r:  SType       ) extends SType
case class  SVar   ( n:  Name                        ) extends SType
case class  SFunc  ( a:  SType     , r:  SType       ) extends SType

/**
 * Combine two alpha-equivalence maps, if compatible. For example, if we want to
 * compute p | q === r | s, we compute p === r and q === s, and try to combine
 * the resulting maps to check that they are compatible. alphaEquivCombine
 * computes the compatibility of those two maps, by ensuring that the
 * intersection of the keysets map to the same value in each map. If so, we
 * return the union of the two maps, indicating the alpha equivalence
 * p | q === r | s is true. If the intersection of the keysets do not share
 * common mappings, the alpha equivalences are incompatible and thus
 * p | q !=== r | s even though p === r and q === s. In this case None is
 * returned.
 */
object alphaEquivCombine extends Function2[
    Option[Map[Name, Name]],
    Option[Map[Name, Name]],
    Option[Map[Name, Name]]
  ] {

  def apply(
      a: Option[Map[Name, Name]],
      b: Option[Map[Name, Name]])
    : Option[Map[Name, Name]] = (a, b) match {
    case (Some(am), Some(bm))
      if !(((am.keySet & bm.keySet) map (n => am(n) == bm(n)))
        contains false) => Some(am ++ bm)
    case _ => None
  }
}

object alphaEquivCombineMany extends Function1[
    List[Option[Map[Name, Name]]],
    Option[Map[Name, Name]]
  ] {

  def apply(ms: List[Option[Map[Name, Name]]]): Option[Map[Name, Name]] =
    ms match {
      case Nil          => Some(Map.empty)
      case m :: Nil     => m
      case m :: n :: os => alphaEquivCombineMany(alphaEquivCombine(m, n) :: os)
    }
}

/**
 * Ensure that two names n and m represent equivalent binders in a given alpha
 * equivalence mapping. They represent equivalent binders if
 * (n notin keyset AND m notin valueset) OR (n in keyset AND n mapsto m)
 */
object alphaEquivEnsureBinding extends Function3[
    Option[Map[Name, Name]],
    Name,
    Name,
    Option[Map[Name, Name]]
  ] {

  def apply(
      a: Option[Map[Name, Name]],
      n: Name,
      m: Name)
    : Option[Map[Name, Name]] = a match {
    case Some(assocs)
      if !assocs.contains(n) && !assocs.valuesIterator.contains(m) => a
    case Some(assocs)
      if  assocs.contains(n) &&  assocs(n) == m                    => a
    case _ => None
  }
}

object alphaEquivEnsureManyBindings extends Function2[
    Option[Map[Name, Name]],
    List[(Name, Name)],
    Option[Map[Name, Name]]
  ] {

  def apply(
      a: Option[Map[Name, Name]],
      nms: List[(Name, Name)])
    : Option[Map[Name, Name]] = nms match {
      case Nil          => a
      case (n, m) :: os => alphaEquivEnsureManyBindings(
        alphaEquivEnsureBinding(a, n, m), os)
  }
}
