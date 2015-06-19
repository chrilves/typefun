package typefun.valuelevel

import scala.annotation.tailrec

// Types to values

case class Frac(numerator : Long , denominator : Long)

object Frac {
  def ofLong(n : Long) : Frac = Frac(n, 1)

  def make(n : Long, d : Long) : Frac = {
    val p = pgcd(n,d)
    Frac( n / p , d / p)
  }

  @tailrec
  def pgcd(n : Long, d : Long) : Long = if (n < d) pgcd(d , n) else if (n % d == 0) d else pgcd(d , n % d)

  def pgcm(n : Long, d : Long) : Long = n * d / pgcd(n,d)
}