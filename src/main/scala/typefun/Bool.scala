package typefun

// Boolean

trait Bool {
  type Match[R, t <: R, e <: R] <: R
}

trait True extends Bool {
  type Match[R, t <: R, e <: R] = t
}

trait False extends Bool {
  type Match[R, t <: R, e <: R] = e
}

trait bool_ind[R, t <: R, e <: R] extends Π[Bool, R] {
  final type λ[b <: Bool] = b # Match[R, t , e]
}
