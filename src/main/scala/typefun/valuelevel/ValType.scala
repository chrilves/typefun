package typefun.valuelevel

import typefun._

trait ValType[+T] {
  type value
}


object ValType {
  def apply[T](implicit t : ValType[T]) : ValType[T] = t

  /*
   Booleans
   */

  trait BoolType            extends ValType[Bool] { final type value = Boolean }
  implicit object boolType  extends BoolType {}


  /*
   Compare
   */

  trait CompType           extends ValType[Comp] { final type value = Compare }
  implicit object compType extends CompType {}

  /*
    Nat
   */

  trait NatType extends ValType[Nat] { final type value = Long }
  implicit object natType extends NatType {}

  /*
   NatPos
   */

  trait NatPosType extends ValType[NatPos] { final type value = Long }
  implicit object natPosType extends NatPosType {}

  abstract class OptionalType[A] extends ValType[Optional[A]] {
    val A : ValType[A]
    final type value = Option[A.value]
  }

  /*
   Options
   */

  implicit def optionalType[A](implicit A_ : ValType[A]) = new OptionalType[A] {
    val A = A_
  }

  abstract class PairType[A,B] extends ValType[Pair[A,B]] {
    val A : ValType[A]
    val B : ValType[B]
    final type value = (A.value, B.value)
  }

  /*
   Pairs
   */

  implicit def pairType[A,B](implicit A_ : ValType[A], B_ : ValType[B]) = new PairType[A,B] {
    val A = A_
    val B = B_
  }

  /*
    Sums
   */

  abstract class SumType[A,B] extends ValType[Sum[A,B]] {
    val A : ValType[A]
    val B : ValType[B]
    final type value = Either[A.value, B.value]
  }

  implicit def sumType[A,B](implicit A_ : ValType[A], B_ : ValType[B]) = new SumType[A,B] {
    val A = A_
    val B = B_
  }
}
