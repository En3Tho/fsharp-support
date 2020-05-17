namespace global
[<AutoOpen>]
module Nested1 =
    let x = 123

namespace Ns
module Nested2 =
    Nested1.x
    ()
