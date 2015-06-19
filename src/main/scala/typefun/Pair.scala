package typefun

// Pair

trait Pair[A,B] {
  type Match[R, f[_ <:A , _<:B] <: R] <: R
  type MatchΠ[R, f <: Π2[A,B,R]] <: R
}

trait apair[A,B, a <: A, b <: B] extends Pair[A,B] {
  final type Match[R, f[_ <:A , _<:B] <: R] = f[a,b]
  final type MatchΠ[R, f <: Π2[A,B,R]] = f # λ2[a,b]
}

trait pair_ind[A,B,R, f[_ <: A, _ <: B] <: R] extends Π[Pair[A,B] , R] {
  final type λ[p <: Pair[A,B]] = p # Match[R, f]
}

trait pair_indΠ[A,B,R, f <: Π2[A,B,R]] extends Π[Pair[A,B] , R] {
  final type λ[p <: Pair[A,B]] = p # MatchΠ[R, f]
}

class swap_[A,B] extends Π2[A,B,Pair[B,A]] {
  final type λ2[a<:A, b<:B] = apair[B,A,b,a]
}