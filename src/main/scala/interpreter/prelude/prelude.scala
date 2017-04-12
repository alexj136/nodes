package interpreter.prelude

import syntax._
import interpreter._

object withDefaultPrelude extends Function1
  [ (Map[Name, String], NumName, Proc)
  , (Map[Name, String], NumName, Proc)
  ] {

  def apply(args: (Map[Name, String], NumName, Proc)):
      (Map[Name, String], NumName, Proc) = {

    val names: Map[Name, String] = args._1
    var nn   : NumName           = args._2
    val proc : Proc              = args._3

    def n(x: Int): NumName = NumName(nn.id + x)
    def v(x: Int): Exp     = Variable(n(x))

    ( names
    , n(2)
    , New
      ( n(0)
      , SChan
        ( List((n(1), List.empty))
        , List(SList(SVar(n(1), List.empty)))
        )
      , ???
      )
    )
  }
}
