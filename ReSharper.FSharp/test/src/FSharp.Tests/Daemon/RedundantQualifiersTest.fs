﻿namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Features.Daemon

open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Tests.Features.Daemon
open JetBrains.ReSharper.TestFramework
open NUnit.Framework

[<TestPackages("FSharp.Core")>]
type RedundantQualifiersTest() =
    inherit FSharpHighlightingTestBase()

    override x.RelativeTestDataPath = "features/daemon/redundantQualifiers"

    override x.HighlightingPredicate(highlighting, _, _) =
        highlighting :? RedundantQualifierWarning

    [<Test>] member x.``AutoOpen - Assembly 01``() = x.DoNamedTest()
    [<Test>] member x.``AutoOpen - Assembly 02 - Implicit FSharp``() = x.DoNamedTest()

    [<Test>] member x.``AutoOpen - Global 01``() = x.DoNamedTest()

    [<Test>] member x.``AutoOpen - Nested 01``() = x.DoNamedTest()
    [<Test>] member x.``AutoOpen - Nested 02 - Qualified``() = x.DoNamedTest()

    [<Test>] member x.``AutoOpen 01``() = x.DoNamedTest()
    [<Test>] member x.``AutoOpen 02 - Qualified``() = x.DoNamedTest()

    [<Test>] member x.``Namespace 01``() = x.DoNamedTest()
    [<Test>] member x.``Namespace 02 - Qualified``() = x.DoNamedTest()
    [<Test>] member x.``Namespace 03 - Multiple import``() = x.DoNamedTest()
