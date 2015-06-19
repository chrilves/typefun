package typefun

import typefun.valuelevel.Val


object Main extends scala.App {

  /*
    Val[T] gives the value level equivalent of a type level term t
   */

  def check[T](implicit t : Val[T]) : Unit = println(Val[T])

  /*
   Booleans
   */

  check[True]
  check[False]

  /*
   Positive naturals
   */

  // This import defined type aliases for type-level positive naturals from 1 to 100 : _i
  import NatPos._

  check[_3]
  check[_57]

  // Sucessor

  check[_13 # succ ]
  check[_13 # succ # succ ]
  check[_13 # succ # succ # succ ]

  // Addition

  check[ _13 # add [_27] ]
  check[ _13 # add [_27] # add [ _32 ]]
  check[ _13 # add [_27 # succ # add [_52 ] ] # add [ _32 ]]

  // Multiplication

  check[ _13 # mul [_27] ]
  check[ _13 # mul [_27] # add [ _32 ]]
  check[ _13 # mul [_27 # succ # mul [_52 ] ] # mul [ _32 ]]

  // "Big" numbers

  check[ _100 # mul [_99] # mul [_98 ] # mul[_97] # mul[_96] # mul[_95 ] # mul[_94] # mul[_93] # mul[_92 ] # mul[_91] # mul[_90] # mul[_5 ] ]

  /*
   Options
   */

  check[none[NatPos]]
  check[some[NatPos, _17]]
  check[some[Bool, True]]
  check[some[NatPos, _17 # mul[_12]]]

  /*
   Pairs
   */
  import Pair._

  check[apair[NatPos, Bool, _17, True]]
  check[apair[Pair[NatPos, Bool], NatPos, apair[NatPos, Bool, _62, False], _35]]

  /*
   Sums
   */

  check[left[NatPos, Bool, _17]]
  check[right[NatPos, Bool, False]]
  check[left[Sum[NatPos, Bool], NatPos, right[NatPos, Bool, False]]]

}
