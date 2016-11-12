package syntax
import scala.util.parsing.input.Positional

sealed trait SyntaxElement {
  var info: Info = NoInfo
  def setInfo(i: Info): Unit = this.info = i
  def pstr(names: Map[Name, String]): String
  def free: Set[Name]
}

case class Name(val id: Int) extends SyntaxElement {
  def next: Name = new Name(this.id + 1)
  def pstr(names: Map[Name, String]): String = names(this)
  def free: Set[Name] = Set(this)
  override def toString: String = s"Name(${this.id})" // for debugging
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
    case Send       ( ch    , msg , p        ) =>
      s"send ${ch pstr names} : ${msg pstr names} . ${p pstr names}"
    case Receive    ( true  , ch  , bind , p ) =>
      s"server ${ch pstr names} : ${names(bind)} . ${p pstr names}"
    case Receive    ( false , ch  , bind , p ) =>
      s"receive ${ch pstr names} : ${names(bind)} . ${p pstr names}"
    case LetIn      ( bind  , exp , p        ) =>
      s"let ${names(bind)} = ${exp.pstr(names)} . ${p pstr names}"
    case IfThenElse ( exp   , tP  , fP       ) =>
      s"if ${exp pstr names} then ${tP pstr names} else ${fP pstr names} endif"
    case Parallel   ( p     , q              ) =>
      s"[ ${p pstr names} | ${q pstr names} ]"
    case New        ( name  , p              ) =>
      s"new ${names(name)} . ${p pstr names}"
    case End                                   => "end"
  }

  def chanLiterals: Set[Name] = this match {
    case Send       ( e , m , p     ) =>
      e.chanLiterals union m.chanLiterals union p.chanLiterals
    case IfThenElse ( e , p , q     ) =>
      e.chanLiterals union p.chanLiterals union q.chanLiterals
    case Receive    ( _ , e , _ , p ) => e.chanLiterals union p.chanLiterals
    case LetIn      ( _ , e , p     ) => e.chanLiterals union p.chanLiterals
    case Parallel   ( p , q         ) => p.chanLiterals union q.chanLiterals
    case New        ( n , p         ) => p.chanLiterals
    case End                          => Set.empty
  }

  def free: Set[Name] = this match {
    case Send       ( e , m , p     ) => e.free union m.free union p.free
    case Receive    ( _ , e , n , p ) => e.free union (p.free - n)
    case LetIn      ( n , e , p     ) => e.free union (p.free - n)
    case IfThenElse ( e , p , q     ) => e.free union p.free union q.free
    case Parallel   ( p , q         ) => p.free union q.free
    case New        ( n , p         ) => p.free - n
    case End                          => Set.empty
  }

  /** Decompose top-level parallel compositions into a list of processes.
   */
  def listify: List[Proc] = this match {
    case Parallel ( p , q ) => p.listify ++ q.listify
    case End                => Nil
    case _                  => List(this)
  }

  /** Alpha-equivalence for processes. If the processes are alpha-equivalent, a
   *  Some of a map containing the 'name equlivalences' between this and that
   *  are returned in an Option. If they are not alpha equivalent, None is
   *  returned.
   */
  def alphaEquiv(that: Proc): Option[Map[Name, Name]] = (this, that) match {
    case ( Send       ( c , e , p     ) , Send       ( d , f , q     ) ) =>
      alphaEquivCombine(alphaEquivCombine(
        c alphaEquiv d, e alphaEquiv f), p alphaEquiv q)
    case ( Receive    ( r , c , e , p ) , Receive    ( s , d , f , q ) ) =>
      if (r == s) alphaEquivCombine(c alphaEquiv d,
        alphaEquivEnsureBinding(p alphaEquiv q, e, f)) else None
    case ( LetIn      ( n , x , p     ) , LetIn      ( m , y , q     ) ) =>
      alphaEquivCombine(x alphaEquiv y,
        alphaEquivEnsureBinding(p alphaEquiv q, n, m))
    case ( IfThenElse ( a , p , r     ) , IfThenElse ( b , q , s     ) ) =>
      alphaEquivCombine(alphaEquivCombine(
        a alphaEquiv b, p alphaEquiv q), r alphaEquiv s)
    case ( Parallel   ( p , r         ) , Parallel   ( q , s         ) ) =>
      alphaEquivCombine(p alphaEquiv q, r alphaEquiv s)
    case ( New        ( n , p         ) , New        ( m , q         ) ) =>
      alphaEquivEnsureBinding(p alphaEquiv q, n, m)
    case ( End                          , End                          ) =>
      Some(Map.empty)
    case ( _                            , _                            ) =>
      None
  }
}

case class  Send      ( ch:   Exp     , msg: Exp  , p:    Proc           )
  extends Proc
case class  Receive   ( repl: Boolean , ch:  Exp  , bind: Name , p: Proc )
  extends Proc
case class  LetIn     ( name: Name    , exp: Exp  , p:    Proc           )
  extends Proc
case class  IfThenElse( exp:  Exp     , tP:  Proc , fP:   Proc           )
  extends Proc
case class  Parallel  ( p:    Proc    , q:   Proc                        )
  extends Proc
case class  New       ( name: Name    , p:   Proc                        )
  extends Proc
case object End
  extends Proc

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
      names getOrElse (name, s"$$<new ${{name.id}}>")
    case IntLiteral  ( value         ) =>
      value.toString
    case BoolLiteral ( value         ) =>
      value.toString
    case ChanLiteral ( name          ) =>
      names getOrElse (name, s"$$<new ${name.id}>")
    case KharLiteral ( value         ) =>
      s"'$value'"
    case Pair        ( l    , r      ) =>
      s"{ ${l pstr names} , ${r pstr names} }"
    case UnExp       ( ty   , of     ) =>
      s"( ${ty.toString} ${of pstr names} )"
    case BinExp      ( ty   , l  , r ) =>
      s"( ${l pstr names} ${ty.toString} ${r pstr names} )"
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
