module Module

open System

type String with
    member x.Foo = 123

type Int32 with
    member x.Foo = 123

let s = ""
let i = 123

s.Foo
s.{caret}Foo
i.Foo
i.Foo