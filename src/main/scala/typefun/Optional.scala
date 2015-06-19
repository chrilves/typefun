package typefun

// Optional


trait Optional[A] {
  type Match[R, default <: R, s <: Π[A, R]] <: R
}

trait none[A] extends Optional[A] {
  final type Match[R, default <: R, s <: Π[A, R]] = default
}

trait some[A, a <: A] extends Optional[A] {
  final type Match[R, default <: R, s <: Π[A, R]] = s#λ[a]
}

trait optional_ind[A,R, default <: R, s <: Π[A, R]] extends Π[Optional[A], R] {
  final type λ[o <: Optional[A]] = o # Match[R, default, s]
}