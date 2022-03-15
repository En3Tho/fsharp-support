namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util

//type IFSharpInferredObjGenericAnalyzer =
//    inherit IElementProblemAnalyzer
//
//[<ElementProblemAnalyzer(typeof<IPrefixAppExpr>, HighlightingTypes = [| |])>]
//type InferredObjGenericAnalyzer() =
//    inherit ElementProblemAnalyzer<IPrefixAppExpr>()
//
//    override this.Run(prefixAppExpr, data, consumer) =
//
//        match prefixAppExpr.FirstChild with
//        | :? IReferenceExpr as functionExpr when functionExpr.Reference.HasFcsSymbol ->
//            let symbol = functionExpr.Reference.GetFcsSymbol()
//
//            let args = functionExpr.TypeArguments
//
//            match symbol with
//            | :? FSharpMemberOrFunctionOrValue as value ->
//                // not sure how to check if generic parameter is automatically inferred or not
//                // also, not sure how to get generic parameters from mfv at all
//                // because it just references the original function
//                let y = 0
//                ()
//            | _ ->
//                ()
//        | _ ->
//            ()
//
//        let x = 0
//        ()
//
//    interface IFSharpInferredObjGenericAnalyzer