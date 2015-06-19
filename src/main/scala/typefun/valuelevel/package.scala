package typefun

import scala.reflect.runtime.universe._

package object valuelevel {
  final def tpe[T : TypeTag] : String = typeOf[T].toString
}
