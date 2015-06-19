package typefun.valuelevel

trait Num[A] {
  def ofLong(n : Long) : A

  def add(a1 : A , a2: A) : A
  def sub(a1 : A , a2: A) : A
  def mul(a1 : A , a2: A) : A
  def div(a1 : A , a2: A) : A
  def inverse(a : A) : A
}

object Num {
  def apply[A](n : Num[A]) : Num[A] = n

  def ofLong[A](n : Long)(implicit A : Num[A]) : A = A.ofLong(n)

  implicit val fracIsNum = new Num[Frac] {
    def ofLong(n : Long) = Frac(n, 1)
    def add(a1: Frac, a2: Frac): Frac = Frac.make(a1.numerator * a2.denominator + a2.numerator * a1.denominator, a1.denominator * a2.denominator)
    def sub(a1: Frac, a2: Frac): Frac = Frac.make(a1.numerator * a2.denominator + a2.numerator * a1.denominator, a1.denominator * a2.denominator)
    def mul(a1: Frac, a2: Frac): Frac = Frac.make(a1.numerator * a2.numerator , a1.denominator * a2.denominator)
    def div(a1: Frac, a2: Frac): Frac = Frac.make(a1.numerator * a2.denominator , a1.denominator * a2.numerator)
    def inverse(a: Frac): Frac = Frac.make(a.denominator, a.numerator)
  }

  implicit val doubleIsNum = new Num[Double] {
    def ofLong(n : Long) = n.toDouble
    def add(a1: Double, a2: Double): Double = a1 + a2
    def sub(a1: Double, a2: Double): Double = a1 - a2
    def mul(a1: Double, a2: Double): Double = a1 * a2
    def div(a1: Double, a2: Double): Double = a1 / a2
    def inverse(a: Double): Double = 1d / a
  }
}

final class NumOps[A](self : A)(implicit A : Num[A]) {
  import A._

  def |+|(a : A) : A = add(self, a)
  def |-|(a : A) : A = sub(self, a)
  def |*|(a : A) : A = mul(self, a)
  def |/|(a : A) : A = div(self, a)
  def inv : A = inverse(self)
}

object NumOps {
  def apply[A](implicit n : NumOps[A]) : NumOps[A] = n

  implicit def ToNumOps[A](self : A)(implicit n : Num[A]) : NumOps[A] = new NumOps[A](self)
}