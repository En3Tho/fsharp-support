module Module

type MyType() =
    member val Member = 1 with get, set

let f() =
    let myType = MyType()
    myType.Member = 5{caret}
    ()
