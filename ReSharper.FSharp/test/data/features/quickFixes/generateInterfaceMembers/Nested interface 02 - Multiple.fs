module Module

type A =
  abstract P1: int

type B =
  inherit A
  abstract P2: int

type C =
  inherit B
  abstract P3: int

type T() =
  interface C{caret}
