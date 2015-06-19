package typefun

import typefun.valuelevel.Num

import scala.annotation.tailrec

// Positive Rationals

trait RatPos {
  type This <: RatPos

  type MatchRatPos[R, inf <: Π[RatInf, R], one <: R , sup <: Π[RatSup , R]] <: R
}

object RatPos {
  def apply(p: Long, q : Long): String =
    if (p == q) "RatOne"
    else if (p > q) {
      if (p % q == 0) s"RatSupOne[${NatPos((p / q) - 1)}]"
      else            s"RatSupRat[${NatPos(p / q)}, ${apply(p % q , q)}]"
    } else {

      if (q % p == 0) s"RatInfOne[${NatPos(q / p - 1)}]"
      else            s"RatInfRat[${NatPos(q / p)}, ${apply(p , q % p)}]"
    }
}

trait RatOne extends RatPos {
  final type This = RatOne

  final type MatchRatPos[R, inf <: Π[RatInf, R], one <: R , sup <: Π[RatSup , R]] = one
}

trait RatInf extends RatPos {
  type This <: RatInf

  final type MatchRatPos[R, inf <: Π[RatInf, R], one <: R, sup <: Π[RatSup, R]] = inf#λ[This]

  type MatchRatInf[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatSup, R]] <: R
}

trait RatSup extends RatPos {
  type This <: RatSup

  final type MatchRatPos[R, inf <: Π[RatInf, R], one <: R, sup <: Π[RatSup, R]] = sup#λ[This]

  type MatchRatSup[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatInf, R]] <: R
}

trait ratpos_ind[R, inf <: Π[RatInf, R], one <: R, sup <: Π[RatSup, R]] extends Π[RatPos, R] {
  final type λ[r <: RatPos] = r # MatchRatPos[R, inf, one, sup]
}

// Rationals 0 < r < 1

trait RatInfOne[n <: NatPos] extends RatInf {
  final type This = RatInfOne[n]

  final type MatchRatInf[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatSup, R]] = f_int # λ[n]
}

trait RatInfRat[n <: NatPos, r <: RatSup] extends RatInf {
  final type This = RatInfRat[n,r]

  final type MatchRatInf[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatSup, R]] = f_rat # λ2[n, r]
}

trait ratinf_ind[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatSup, R]] extends Π[RatInf, R] {
  final type λ[i <: RatInf] = i # MatchRatInf[R, f_int, f_rat]
}


// rationals 1 < r

trait RatSupOne[n <: NatPos] extends RatSup {
  final type This = RatSupOne[n]

  final type MatchRatSup[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatInf, R]] = f_int # λ[n]
}

trait RatSupRat[n <: NatPos, r <: RatInf] extends RatSup {
  final type This = RatSupRat[n,r]

  final type MatchRatSup[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatInf, R]] = f_rat # λ2[n, r]
}

trait ratsup_ind[R, f_int <: Π[NatPos, R], f_rat <: Π2[NatPos, RatInf, R]] extends Π[RatSup, R] {
  final type λ[i <: RatSup] = i # MatchRatSup[R, f_int, f_rat]
}
/*
// From RatPos types to values

sealed trait RatPosNum[A, q <: RatPos] {
  val value : A
}

object RatPosNum{
  def apply[A, q <: RatPos](implicit A : Num[A], q : RatPosNum[A, q]) : A = q.value


  implicit final def one[A](implicit A : Num[A]) = new RatPosNum[A, RatOne] {
    val value = A.ofLong(1)
  }

  implicit final def supone[A, n <: NatPos](implicit A : Num[A], n : NatPosToLong[n]) = new RatPosNum[A, RatSupOne[n]] {
    val value = A.ofLong(n.value + 1)
  }

  implicit final def infone[A, n <: NatPos](implicit A : Num[A], n : NatPosToLong[n]) = new RatPosNum[A, RatInfOne[n]] {
    val value = A.inverse(A.ofLong(n.value + 1))
  }

  implicit final def suprat[A, n <: NatPos, r <: RatInf](implicit A : Num[A], n : NatPosToLong[n], r : RatPosNum[A, r]) = new RatPosNum[A, RatSupRat[n, r]] {
    val value = A.add(A.ofLong(n.value) , r.value)
  }

  implicit final def infrat[A, n <: NatPos, r <: RatSup](implicit A : Num[A], n : NatPosToLong[n], r : RatPosNum[A, r]) = new RatPosNum[A, RatInfRat[n, r]] {
    val value = A.inverse(A.add(A.ofLong(n.value) , A.inverse(r.value)))
  }
}*/