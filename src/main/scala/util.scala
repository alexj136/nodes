package util

sealed abstract class NonEmptyList[+T] {
  def map[U](f: T => U): NonEmptyList[U] = this match {
    case Last(x) => Last(f(x))
    case Node(x, l) => Node(f(x), l map f)
  }
  def head: T
  def tail: NonEmptyList[T]
}

case class Last[T](elem: T) extends NonEmptyList[T] {
  override def head = elem
  override def tail = throw new NoSuchElementException("Tail of singleton list")
}

case class Node[T](elem: T, rest: NonEmptyList[T]) extends NonEmptyList[T] {
  override def head = elem
  override def tail = rest
}

object NonEmptyList {
  def apply[T](elems: T*): NonEmptyList[T] =
    if (elems.length == 1) Last(elems.head)
    else if (elems.length > 1) Node(elems.head, apply(elems.tail: _*))
    else throw new IllegalArgumentException
}
