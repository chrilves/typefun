package typefun

// NatPos

trait NatPos {
  type This <: NatPos

  type Match[R, one <: R, t0_[_ <: NatPos] <: R, t1_[_ <: NatPos] <: R] <: R
  type chain[R, F[_ <: NatPos] <: R] <: R

  type MatchΠ[R, one <: R, t0_ <: Π[NatPos, R], t1_ <: Π[NatPos, R]] <: R
  type chainΠ[R, f <: Π[NatPos,R]] <: R

  type succ <: NatPos

  type add[m <: NatPos] <: NatPos

  type mul[m <: NatPos] <: NatPos

  //type exp[m <: NatPos] <: NatPos

  type compare[m <: NatPos] <: Comp
}

trait I extends NatPos {
  type This = I

  final type Match[R, one <: R, t0_[_ <: NatPos] <: R, t1_[_ <: NatPos] <: R] = one
  final type chain[R, F[_ <: NatPos] <: R] = F[I]

  final type MatchΠ[R, one <: R, t0_ <: Π[NatPos, R], t1_ <: Π[NatPos, R]] = one
  final type chainΠ[R, f <: Π[NatPos, R]] = f#λ[I]

  final type succ = T0[I]

  final type add[m <: NatPos] = m # succ

  final type mul[m <: NatPos] = m

  //final type exp[m <: NatPos] = I

  final type compare[m <: NatPos] = m # MatchΠ[Comp, EQ, cst[NatPos, Comp, LT], cst[NatPos, Comp, LT]]
}

trait T0[n <: NatPos] extends NatPos {

  final type This = T0[n]

  final type Match[R, one <: R, t0_[_ <: NatPos] <: R, t1_[_ <: NatPos] <: R] = t0_[n]
  final type chain[R, F[_ <: NatPos] <: R] = F[T0[n]]

  final type MatchΠ[R, one <: R, t0_ <: Π[NatPos, R], t1_ <: Π[NatPos, R]] = t0_ # λ [ n ]
  final type chainΠ[R, f <: Π[NatPos, R]] = f#λ[T0[n]]

  final type succ = T1[n]

  // ADD

  final type add[m <: NatPos] = m # Match[NatPos, T1[n] , fun1 , fun2 ]

  final type fun1[m2 <: NatPos] = m2 # add[n] # chain[NatPos, T0]
  final type fun2[m2 <: NatPos] = m2 # add[n] # chain[NatPos, T1]

  // Mul

  final type mul[m <: NatPos] = m # Match[NatPos , This , fun3 , fun4 ]

  final type fun3[m2 <: NatPos] = ({type f[x <: NatPos] = T0[T0[x]]})#f[m2 # mul[n]]
  final type fun4[m2 <: NatPos] = ({type f[x <: NatPos] = x # add [n] # chain[NatPos, T0]})#f[m2 # mul[n] # chain[NatPos, T0]]

/*
  // Exp

  type exp[m <: NatPos] = m # Match[NatPos, This, fun7, fun8]

  final type fun7[m2 <: NatPos] = This # mul[This] # exp[m2]
  final type fun8[m2 <: NatPos] = This # mul[This] # exp[m2] # mul[This]
*/
  // Compare

  final type compare[m <: NatPos] = m # Match[Comp, GT , fun5 , fun6 ]

  final type fun5[m2 <: NatPos] = n # compare[m2]
  final type fun6[m2 <: NatPos] = n # compare[m2] # Match[Comp, LT, LT, GT]
}

trait T1[n <: NatPos] extends NatPos {

  final type This = T1[n]

  final type Match[R, one <: R, t0_[_ <: NatPos] <: R, t1_[_ <: NatPos] <: R] = t1_[n]
  final type chain[R, F[_ <: NatPos] <: R] = F[T1[n]]

  final type MatchΠ[R, one <: R, t0_ <: Π[NatPos, R] , t1_ <: Π[NatPos, R]] = t1_ # λ [n]
  final type chainΠ[R, f <: Π[NatPos, R]] = f#λ[T1[n]]

  final type succ = T0[n#succ]

  // Add

  final type add[m <: NatPos] = m # Match[ NatPos, exp1 , fun1, fun2 ]

  final type exp1 = T0[n # succ]

  final type fun1[m2 <: NatPos] = m2 # add[n] # chain[NatPos, T1]
  final type fun2[m2 <: NatPos] = m2 # add[n] # succ # chain[NatPos, T0]

  // Mul

  final type mul[m <: NatPos] = m # Match[NatPos , This , fun3 , fun4 ]

  final type fun3[m2 <: NatPos] = ({ type f[x <: NatPos] = x # add [ m2 ] # chain[NatPos, T0]})#f[m2 # mul[n] # chain[NatPos, T0]]
  final type fun4[m2 <: NatPos] = ({ type f[x <: NatPos] = x # add [ m2 # add[n] # chain[NatPos, T1] ] })#f[m2 # mul[n] # chain[NatPos, ({ type G[x <: NatPos] = T0[T0[x]]})#G]]
/*
  // Exp

  type exp[m <: NatPos] = m # Match[NatPos, This, fun7, fun8]

  final type fun7[m2 <: NatPos] = This # mul[This] # exp[m2]
  final type fun8[m2 <: NatPos] = This # mul[This] # exp[m2] # mul[This]
*/
  // Compare

  final type compare[m <: NatPos] = m # Match[Comp, GT , fun5 , fun6 ]

  final type fun5[m2 <: NatPos] = n # compare[m2] # Match[Comp, LT, GT, GT]
  final type fun6[m2 <: NatPos] = n # compare[m2]
}


trait natpos_ind[R, one <: R, t0_ <: Π[NatPos, R], t1_ <: Π[NatPos, R]] extends Π[NatPos, R] {
  final type λ[n <: NatPos] = n # MatchΠ[R, one, t0_, t1_]
}

trait ReflectNatPos extends NatPos {
  type This <: NatPos
  final type MatchΠ[R, one <: R, t0_ <: Π[NatPos, R], t1_ <: Π[NatPos, R]] = This # MatchΠ[R, one, t0_ , t1_ ]
}

/*
As functions
*/

trait t0Π extends Π[NatPos, NatPos] {
  final type λ[n <: NatPos] = T0[n]
}

trait t1Π extends Π[NatPos, NatPos] {
  final type λ[n <: NatPos] = T1[n]
}

trait succ extends Π[NatPos, NatPos] {
  final type λ[n <: NatPos] = n # succ
}

trait add extends Π2[NatPos, NatPos, NatPos] {
  final type λ2[n <: NatPos, m <: NatPos] = n # add[m]
}

trait mul extends Π2[NatPos, NatPos, NatPos] {
  final type λ2[n <: NatPos, m <: NatPos] = n # mul[m]
}

object NatPos {
  def apply(l : Long) : String =
    if (l == 1) "I"
    else if (l % 2 == 0) s"T0[${apply(l/2)}]"
    else            s"T1[${apply(l/2)}]"

  type _1 = I
  type _2 = T0[I]
  type _3 = T1[I]
  type _4 = T0[T0[I]]
  type _5 = T1[T0[I]]
  type _6 = T0[T1[I]]
  type _7 = T1[T1[I]]
  type _8 = T0[T0[T0[I]]]
  type _9 = T1[T0[T0[I]]]
  type _10 = T0[T1[T0[I]]]
  type _11 = T1[T1[T0[I]]]
  type _12 = T0[T0[T1[I]]]
  type _13 = T1[T0[T1[I]]]
  type _14 = T0[T1[T1[I]]]
  type _15 = T1[T1[T1[I]]]
  type _16 = T0[T0[T0[T0[I]]]]
  type _17 = T1[T0[T0[T0[I]]]]
  type _18 = T0[T1[T0[T0[I]]]]
  type _19 = T1[T1[T0[T0[I]]]]
  type _20 = T0[T0[T1[T0[I]]]]
  type _21 = T1[T0[T1[T0[I]]]]
  type _22 = T0[T1[T1[T0[I]]]]
  type _23 = T1[T1[T1[T0[I]]]]
  type _24 = T0[T0[T0[T1[I]]]]
  type _25 = T1[T0[T0[T1[I]]]]
  type _26 = T0[T1[T0[T1[I]]]]
  type _27 = T1[T1[T0[T1[I]]]]
  type _28 = T0[T0[T1[T1[I]]]]
  type _29 = T1[T0[T1[T1[I]]]]
  type _30 = T0[T1[T1[T1[I]]]]
  type _31 = T1[T1[T1[T1[I]]]]
  type _32 = T0[T0[T0[T0[T0[I]]]]]
  type _33 = T1[T0[T0[T0[T0[I]]]]]
  type _34 = T0[T1[T0[T0[T0[I]]]]]
  type _35 = T1[T1[T0[T0[T0[I]]]]]
  type _36 = T0[T0[T1[T0[T0[I]]]]]
  type _37 = T1[T0[T1[T0[T0[I]]]]]
  type _38 = T0[T1[T1[T0[T0[I]]]]]
  type _39 = T1[T1[T1[T0[T0[I]]]]]
  type _40 = T0[T0[T0[T1[T0[I]]]]]
  type _41 = T1[T0[T0[T1[T0[I]]]]]
  type _42 = T0[T1[T0[T1[T0[I]]]]]
  type _43 = T1[T1[T0[T1[T0[I]]]]]
  type _44 = T0[T0[T1[T1[T0[I]]]]]
  type _45 = T1[T0[T1[T1[T0[I]]]]]
  type _46 = T0[T1[T1[T1[T0[I]]]]]
  type _47 = T1[T1[T1[T1[T0[I]]]]]
  type _48 = T0[T0[T0[T0[T1[I]]]]]
  type _49 = T1[T0[T0[T0[T1[I]]]]]
  type _50 = T0[T1[T0[T0[T1[I]]]]]
  type _51 = T1[T1[T0[T0[T1[I]]]]]
  type _52 = T0[T0[T1[T0[T1[I]]]]]
  type _53 = T1[T0[T1[T0[T1[I]]]]]
  type _54 = T0[T1[T1[T0[T1[I]]]]]
  type _55 = T1[T1[T1[T0[T1[I]]]]]
  type _56 = T0[T0[T0[T1[T1[I]]]]]
  type _57 = T1[T0[T0[T1[T1[I]]]]]
  type _58 = T0[T1[T0[T1[T1[I]]]]]
  type _59 = T1[T1[T0[T1[T1[I]]]]]
  type _60 = T0[T0[T1[T1[T1[I]]]]]
  type _61 = T1[T0[T1[T1[T1[I]]]]]
  type _62 = T0[T1[T1[T1[T1[I]]]]]
  type _63 = T1[T1[T1[T1[T1[I]]]]]
  type _64 = T0[T0[T0[T0[T0[T0[I]]]]]]
  type _65 = T1[T0[T0[T0[T0[T0[I]]]]]]
  type _66 = T0[T1[T0[T0[T0[T0[I]]]]]]
  type _67 = T1[T1[T0[T0[T0[T0[I]]]]]]
  type _68 = T0[T0[T1[T0[T0[T0[I]]]]]]
  type _69 = T1[T0[T1[T0[T0[T0[I]]]]]]
  type _70 = T0[T1[T1[T0[T0[T0[I]]]]]]
  type _71 = T1[T1[T1[T0[T0[T0[I]]]]]]
  type _72 = T0[T0[T0[T1[T0[T0[I]]]]]]
  type _73 = T1[T0[T0[T1[T0[T0[I]]]]]]
  type _74 = T0[T1[T0[T1[T0[T0[I]]]]]]
  type _75 = T1[T1[T0[T1[T0[T0[I]]]]]]
  type _76 = T0[T0[T1[T1[T0[T0[I]]]]]]
  type _77 = T1[T0[T1[T1[T0[T0[I]]]]]]
  type _78 = T0[T1[T1[T1[T0[T0[I]]]]]]
  type _79 = T1[T1[T1[T1[T0[T0[I]]]]]]
  type _80 = T0[T0[T0[T0[T1[T0[I]]]]]]
  type _81 = T1[T0[T0[T0[T1[T0[I]]]]]]
  type _82 = T0[T1[T0[T0[T1[T0[I]]]]]]
  type _83 = T1[T1[T0[T0[T1[T0[I]]]]]]
  type _84 = T0[T0[T1[T0[T1[T0[I]]]]]]
  type _85 = T1[T0[T1[T0[T1[T0[I]]]]]]
  type _86 = T0[T1[T1[T0[T1[T0[I]]]]]]
  type _87 = T1[T1[T1[T0[T1[T0[I]]]]]]
  type _88 = T0[T0[T0[T1[T1[T0[I]]]]]]
  type _89 = T1[T0[T0[T1[T1[T0[I]]]]]]
  type _90 = T0[T1[T0[T1[T1[T0[I]]]]]]
  type _91 = T1[T1[T0[T1[T1[T0[I]]]]]]
  type _92 = T0[T0[T1[T1[T1[T0[I]]]]]]
  type _93 = T1[T0[T1[T1[T1[T0[I]]]]]]
  type _94 = T0[T1[T1[T1[T1[T0[I]]]]]]
  type _95 = T1[T1[T1[T1[T1[T0[I]]]]]]
  type _96 = T0[T0[T0[T0[T0[T1[I]]]]]]
  type _97 = T1[T0[T0[T0[T0[T1[I]]]]]]
  type _98 = T0[T1[T0[T0[T0[T1[I]]]]]]
  type _99 = T1[T1[T0[T0[T0[T1[I]]]]]]
  type _100 = T0[T0[T1[T0[T0[T1[I]]]]]]
}