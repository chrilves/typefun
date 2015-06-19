package typefun.valuelevel

import typefun._

trait Val[+T] extends ValType[T] {
  val  value : value
}


object Val {
  import ValType._

  def apply[T](implicit v : Val[T]) : v.value = v.value

  /*
   Booleans
   */


  sealed trait BoolVal[b <: Bool]  extends Val[b] with BoolType
  implicit object valTrue   extends BoolVal[True ] { val  value = true  }
  implicit object valFalse  extends BoolVal[False] { val  value = false }

  /*
   Compare
   */


  trait CompVal[c <: Comp] extends Val[c] with CompType
  implicit object lt extends CompVal[LT] { val value  = Lt }
  implicit object eq extends CompVal[EQ] { val value  = Eq }
  implicit object gt extends CompVal[GT] { val value  = Gt }

  /*
    Nat
   */

  sealed trait NatVal[n <: Nat] extends Val[n] with NatType
  implicit object zero extends NatVal[Z] { val value = 0L }
  implicit def succ[m <: Nat](implicit m : NatVal[m]) = new NatVal[S[m]] { val value = m.value + 1L }

  /*
   NatPos
   */

  sealed trait NatPosVal[n <: NatPos] extends Val[n] with NatPosType
  implicit final object one extends NatPosVal[I] { val value = 1L }
  implicit final def odd[n <: NatPos](implicit n : NatPosVal[n]) = new NatPosVal[T0[n]] { val value = 2 * n.value }
  implicit final def even[n <: NatPos](implicit n : NatPosVal[n]) = new NatPosVal[T1[n]] { val value = 2 * n.value + 1}


  /*
   Options
   */


  sealed abstract class OptionalVal[A, o <: Optional[A]] extends OptionalType[A] with Val[o]

  implicit def noneval[A](implicit A_ : ValType[A]) = new OptionalVal[A, none[A]]{
    val A = A_
    val value = None
  }

  implicit def someval[A, a <: A](implicit a : Val[a]) = new OptionalVal[A, some[A,a]] {
    val A = a
    val value = Some(A.value)
  }


  /*
   Pairs
   */

  sealed abstract class PairTypeVal[A,B, p <: Pair[A,B]] extends PairType[A,B] with Val[p]

  implicit def apairType[A,B,a<:A,b<:B](implicit a : Val[a], b : Val[b]) = new PairTypeVal[A,B,apair[A,B,a,b]] {
    val A = a
    val B = b
    val value = (A.value, B.value)
  }


  /*
 Sums
 */

  sealed abstract class SumVal[A,B, p <: Sum[A,B]] extends SumType[A,B] with Val[p]

  implicit def leftType[A,B,a<:A](implicit a : Val[a], B_ : ValType[B]) = new SumVal[A,B,left[A,B,a]] {
    val A = a
    val B = B_
    val value = Left(A.value)
  }

  implicit def rightType[A,B,b<:B](implicit A_ : ValType[A], b : Val[b]) = new SumVal[A,B,right[A,B,b]] {
    val A = A_
    val B = b
    val value = Right(B.value)
  }

}