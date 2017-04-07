package typecheck.typeclass_typecheck

import syntax._
import typecheck._

class TypeClassTypecheck ( nextName: NumName ) extends Typecheck ( nextName ) {

  override def unify
    ( constrs: ConstraintSet
    , failed:  ConstraintSet
    ): Either [ ConstraintSet , SType => SType ] = constrs.split match {
      case (None, _) => if (failed.isEmpty) Right(identity) else Left(failed)
      case _         => ???
  }
}
