package interpreter.prelude

import syntax._
import interpreter._

object withDefaultPrelude
    extends Function1[(Map[Name, String], NumName, Proc), Proc] {

  def apply(args: (Map[Name, String], NumName, Proc)) = {
    val names : Map[Name, String] = args._1
    val nn    : NumName           = args._2
    val proc  : Proc              = args._3

    ???
  }
}
