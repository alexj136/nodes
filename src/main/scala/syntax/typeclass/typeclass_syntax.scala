package syntax.typeclass_syntax

import syntax._

sealed abstract class TypeClassElement extends SyntaxElement

case class TypeClassDecl
  ( val tcName : Name
  , val bind   : Name
  , val ty     : SChan
  , val body   : TypeClassElement
  ) extends TypeClassElement {

  override def pstr(names: Map[Name, String]) =
    s"class ${tcName pstr names} ${bind pstr names} = ${ty pstr names} in " +
    s"${body pstr names}"

  override def free: Set[Name] =
    (body.free union (ty.free - bind)) - tcName
}

case class TypeClassInst
  ( val tcName  : Name
  , val instTy  : SType
  , val witness : Receive
  , val body    : TypeClassElement
  ) extends TypeClassElement {

  override def pstr(names: Map[Name, String]) =
    s"instance ${tcName pstr names} ${instTy pstr names} where\n" +
    s"    ${witness pstr names}\n" +
    s"in\n" +
    s"    ${body pstr names}"

  override def free: Set[Name] =
    body.free union witness.free union instTy.free + tcName
}

case class TypeClassProc ( p: Proc ) extends TypeClassElement {
  override def pstr(names: Map[Name, String]) = p pstr names
  override def free: Set[Name] = p.free
}
