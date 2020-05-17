module TopLevel =
    [<AutoOpen>]
    module Nested =
        let x = 123

open TopLevel
Nested.x
