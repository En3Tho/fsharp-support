[<AbstractClass>]
type A<'T1>() =
    abstract M: 'T1 -> unit
    abstract P: int

[<AbstractClass>]
type B<'T2>() =
    inherit A<'T2>()

    override x.M _ = ()

type C{caret}() =
    inherit B<int>()
