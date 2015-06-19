package typefun

trait Nat {
  type This <: Nat
  type chain[R,  F[_ <: Nat] <: R] <: R
  type chainΠ[R,  F <: Π[Nat, R]] <: R
}

trait Z extends Nat {
  type This = Z
  type chain[R, F[_ <: Nat] <: R] = F[Z]
  type chainΠ[R,  F <: Π[Nat, R]] = F#λ[Z]
}

trait S[n <: Nat] extends Nat {
  type This = S[n]
  type chain[R, F[_ <: Nat] <: R] = F[S[n]]
  type chainΠ[R,  F <: Π[Nat, R]] = F#λ[S[n]]
}