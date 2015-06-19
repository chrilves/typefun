package typefun.valuelevel

import typefun._

sealed abstract class  Compare { type t <: Comp }
case object Lt extends Compare { type t = LT    }
case object Eq extends Compare { type t = EQ    }
case object Gt extends Compare { type t = GT    }

