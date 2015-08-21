package syntax

class Name(id: Int) {
  def next: Name = new Name(this.id + 1)
}

sealed abstract class Proc {
  def pstr(names: Map[Name, String]): String = this match {
    case Send       ( ch    , msg , p        ) =>
      s"${ch pstr names}![${msg pstr names}].${p pstr names}"
    case Receive    ( true  , ch  , bind , p ) =>
      s"${ch pstr names}?*[${names(bind)}].${p pstr names}"
    case Receive    ( false , ch  , bind , p ) =>
      s"${ch pstr names}?[${names(bind)}].${p pstr names}"
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

  /** Decompose top-level parallel compositions into a list of processes.
   */
  def listify: List[Proc] = this match {
    case Parallel ( p , q ) => p.listify ++ q.listify
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

sealed abstract class Exp {
  def pstr(names: Map[Name, String]): String = this match {
    case Variable    ( name         ) =>
      names(name)
    case IntLiteral  ( value        ) =>
      value.toString
    case BoolLiteral ( value        ) =>
      value.toString
    case ChanLiteral ( name         ) =>
      names(name)
    case BinExp      ( ty   , l , r ) =>
      s"${l pstr names} ${ty.toString} ${r pstr names}"
    case Not         ( of           ) =>
      s"!${of pstr names}"
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
      case ( BinExp      ( a , c , e ) , BinExp      ( b , d , f ) ) =>
        (a == b) && c.syntaxEquiv(thisNames, d, yNames) &&
          e.syntaxEquiv(thisNames, f, yNames)
      case ( Not         ( a         ) , Not         ( b         ) ) =>
        a.syntaxEquiv(thisNames, b, yNames)
      case ( _                         , _                         ) => false
    }
}
case class Variable    ( name:      Name                          ) extends Exp
case class IntLiteral  ( value:     Int                           ) extends Exp
case class BoolLiteral ( value:     Boolean                       ) extends Exp
case class ChanLiteral ( name:      Name                          ) extends Exp
case class BinExp      ( binOpType: BinOp   , lhs: Exp , rhs: Exp ) extends Exp
case class Not         ( of:        Exp                           ) extends Exp

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
