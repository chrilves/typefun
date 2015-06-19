package typefun

// Fun

trait Π[A,B] {
  type λ[α <: A] <: B
}

trait Π2[A,B,C] extends Π[A,Π[B,C]] {
  type λ2[α <: A, β <: B] <: C

  final type λ[α <: A] = Π[B,C] { type λ[β <: B] = λ2[α , β] }
}

trait compose[A,B,C, f <: Π[B,C], g <: Π[A,B]] extends Π[A,C] {
  final type λ[a <: A] = f#λ[ g#λ[a] ]
}

trait cst[A, B, b <: B] extends Π[A, B] {
  final type λ[_ <: A] = b
}


// Thunk

trait Thunk[A] {
  type eval <: A
}

trait Now[A, a <: A] extends Thunk[A] {
  final type eval = a
}

trait App[A, B, f <: Π[A,B], a <: A] extends Thunk[B] {
  final type eval = f # λ[a]
}

trait App_[A, B, f[_ <: A] <:B, a <: A] extends Thunk[B] {
  final type eval = f[a]
}

trait reflect[A,B, f <: Π[A,Thunk[B]] { type λ[α <: A] <: Thunk[B] } ] extends Π[A,B] {
  final type λ[α<:A] = f # λ[α] # eval
}

trait reify[A,B, f <: Π[A,B]] extends Π[A,Thunk[B]] {
  final type λ[α<:A] = App[A,B,f,α]
}

trait AppJ[A, B, f <: Π[A,Thunk[B]] { type λ[α <: A] <: Thunk[B] } , a <: A] extends Thunk[B] {
  final type eval = f # λ[a] # eval
}