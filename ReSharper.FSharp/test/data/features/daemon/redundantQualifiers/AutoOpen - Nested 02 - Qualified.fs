module TopLevel

module Nested1 =
    [<AutoOpen>]
    module Nested2 =
        let x = 123

open Nested1
Nested1.Nested2.x
