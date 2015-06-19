package typefun

// Sums

trait Sum[A,B] {
  type Match[R, l <: Π[A,R], r <: Π[B,R]] <: R
}

trait left[A,B, a <: A] extends Sum[A,B] {
  final type Match[R, l <: Π[A,R], r <: Π[B,R]] = l # λ[a]
}

trait right[A,B, b <: B] extends Sum[A,B] {
  final type Match[R, l <: Π[A,R], r <: Π[B,R]] = r # λ[b]
}

trait sum_ind[A,B,R,  l <: Π[A,R], r <: Π[B,R]] extends Π[Sum[A,B] , R] {
  final type λ[s <: Sum[A,B]] = s # Match[R, l, r]
}