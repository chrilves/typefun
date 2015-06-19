package typefun

trait Comp {
  type Match[R, lt <: R, eq <: R, gt <: R] <: R
}

trait LT extends Comp {
  final type Match[R, lt <: R, eq <: R, gt <: R] = lt
}

trait EQ extends Comp {
  final type Match[R, lt <: R, eq <: R, gt <: R] = eq
}

trait GT extends Comp {
  final type Match[R, lt <: R, eq <: R, gt <: R] = gt
}

trait comp_ind[R, lt <: R, eq <: R, gt <: R] extends Π[Comp, R] {
  final type λ[c <: Comp] = c # Match[R, lt, eq, gt]
}