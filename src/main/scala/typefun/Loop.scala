package typefun

// Loop

trait LoopRec[l <: Π[Any, Thunk[Any]] { type λ[α <: Any] <: Thunk[Any] } ] extends Π[Any,Thunk[Any]] {
  final type λ[α <: Any] = AppJ[Any, Any, l, α]
}

trait Loop extends LoopRec[Loop]

object Loop extends Loop