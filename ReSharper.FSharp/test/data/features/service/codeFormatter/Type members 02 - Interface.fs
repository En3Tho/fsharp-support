﻿module Module

type I =
  abstract member A : unit -> unit

type A(a) =
  interface I with
    member x.A() = ()
