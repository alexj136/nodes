package syntax

sealed trait SyntaxElement {
  var info: Info = NoInfo
  def pstr(names: Map[Name, String]): String
  def free: Set[Name]
}

class Name(val id: Int) {
  def next: Name = new Name(this.id + 1)
}

abstract class Info
case class SrcPosInfo(pos: (Int, Int, Int, Int)) extends Info
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
      s"let ${names(bind)} = ${exp.pstr(names)}.${p pstr names}"
    case IfThenElse ( exp   , tP  , fP       ) =>
      s"if ${exp pstr names} then ${tP pstr names} else ${fP pstr names}"
    case Parallel   ( p     , q              ) =>
      s"(${p pstr names} | ${q pstr names})"
    case New        ( name  , p              ) =>
      s"new ${names(name)}.${p pstr names}"
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

  /** Syntax-equivalence for processes.
   */
  def syntaxEquiv(thisNames: Map[Name, String],
      q: Proc, qNames: Map[Name, String]): Boolean =
    (this, q) match {
      case ( Send       ( c , e , p     ) , Send       ( d , f , q     ) ) =>
        c.syntaxEquiv(thisNames, d, qNames) &&
          e.syntaxEquiv(thisNames, f, qNames) &&
          p.syntaxEquiv(thisNames, q, qNames)
      case ( Receive    ( r , c , e , p ) , Receive    ( s , d , f , q ) ) =>
        (r == s) && c.syntaxEquiv(thisNames, d, qNames) &&
          (thisNames(e) == qNames(f)) &&
          p.syntaxEquiv(thisNames, q, qNames)
      case ( LetIn      ( n , x , p     ) , LetIn      ( m , y , q     ) ) =>
        (thisNames(n) == qNames(m)) &&
          x.syntaxEquiv(thisNames, y, qNames) &&
          p.syntaxEquiv(thisNames, q, qNames)
      case ( IfThenElse ( a , p , r     ) , IfThenElse ( b , q , s     ) ) =>
        a.syntaxEquiv(thisNames, b, qNames) &&
          p.syntaxEquiv(thisNames, q, qNames) &&
          r.syntaxEquiv(thisNames, s, qNames)
      case ( Parallel   ( p , r         ) , Parallel   ( q , s         ) ) =>
        p.syntaxEquiv(thisNames, q, qNames) &&
          r.syntaxEquiv(thisNames, s, qNames)
      case ( New        ( n , p         ) , New        ( m , q         ) ) =>
        p.syntaxEquiv(thisNames, q, qNames) && (thisNames(n) == qNames(m))
      case ( End                          , End                          ) =>
        true
      case ( _                            , _                            ) =>
        false
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
    case UnExp       ( _ , e     ) => e.free
    case BinExp      ( _ , l , r ) => l.chanLiterals union r.chanLiterals
  }

  def free: Set[Name] = this match {
    case Variable    ( n         ) => Set(n)
    case IntLiteral  ( _         ) => Set.empty
    case BoolLiteral ( _         ) => Set.empty
    case ChanLiteral ( _         ) => Set.empty
    case Pair        ( l , r     ) => l.free union r.free
    case UnExp       ( _ , e     ) => e.free
    case BinExp      ( _ , l , r ) => l.free union r.free
  }

  def contains(n: Name): Boolean = this match {
    case Variable    ( m         ) => n == m
    case IntLiteral  ( _         ) => false
    case BoolLiteral ( _         ) => false
    case ChanLiteral ( _         ) => false
    case Pair        ( l , r     ) => (l contains n) || (r contains n)
    case UnExp       ( _ , e     ) => e contains n
    case BinExp      ( _ , l , r ) => (l contains n) || (r contains n)
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
    case Pair        ( l    , r      ) =>
      s"{ ${l pstr names} , ${r pstr names} }"
    case UnExp       ( ty   , of     ) =>
      s"${ty.toString} ${of pstr names}"
    case BinExp      ( ty   , l  , r ) =>
      s"${l pstr names} ${ty.toString} ${r pstr names}"
  }

  /** Syntax-equivalence for expressions
   */
  def syntaxEquiv(thisNames: Map[Name, String],
      y: Exp, yNames: Map[Name, String]): Boolean =
    (this, y) match {
      case ( Variable    ( n         ) , Variable    ( m         ) ) =>
        thisNames(n) == yNames(m)
      case ( IntLiteral  ( a         ) , IntLiteral  ( b         ) ) =>
        a == b
      case ( BoolLiteral ( a         ) , BoolLiteral ( b         ) ) =>
        a == b
      case ( ChanLiteral ( n         ) , ChanLiteral ( m         ) ) =>
        thisNames(n) == yNames(m)
      case ( Pair        ( a , c     ) , Pair        ( b , d     ) ) =>
        a.syntaxEquiv(thisNames, b, yNames) &&
          c.syntaxEquiv(thisNames, d, yNames)
      case ( UnExp       ( a , c     ) , UnExp       ( b , d     ) ) =>
        (a == b) && c.syntaxEquiv(thisNames, d, yNames)
      case ( BinExp      ( a , c , e ) , BinExp      ( b , d , f ) ) =>
        (a == b) && c.syntaxEquiv(thisNames, d, yNames) &&
          e.syntaxEquiv(thisNames, f, yNames)
      case ( _                         , _                         ) => false
    }
}
case class Variable    ( name:      Name                          ) extends Exp
case class IntLiteral  ( value:     Int                           ) extends Exp
case class BoolLiteral ( value:     Boolean                       ) extends Exp
case class ChanLiteral ( name:      Name                          ) extends Exp
case class Pair        ( lhs:       Exp     , rhs: Exp            ) extends Exp
case class UnExp       ( unOpType:  UnOp    , of:  Exp            ) extends Exp
case class BinExp      ( binOpType: BinOp   , lhs: Exp , rhs: Exp ) extends Exp

sealed abstract class BinOp {
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

sealed abstract class UnOp {
  override def toString: String = this match {
    case Not    => "!"
    case PLeft  => "<-"
    case PRight => "->"
  }
}
case object Not    extends UnOp
case object PLeft  extends UnOp
case object PRight extends UnOp
