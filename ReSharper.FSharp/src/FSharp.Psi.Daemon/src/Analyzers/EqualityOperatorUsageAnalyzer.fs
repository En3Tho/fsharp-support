module JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers.EqualityOperatorUsageAnalyzer

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util

type IFSharpEqualityOperatorBoxingAnalyzer =
    inherit IElementProblemAnalyzer

[<ElementProblemAnalyzer(typeof<IBinaryAppExpr>, HighlightingTypes = [| |])>]
type EqualityOperatorUsageAnalyzer() =
    inherit ElementProblemAnalyzer<IBinaryAppExpr>()

    override this.Run(binaryAppExpr, data, consumer) =

        // TODO: code fixer ?

        let isFsharpCoreEqualsOperator (operatorExpr: IReferenceExpr) =
            let symbol = operatorExpr.Reference.GetFcsSymbol()
            isNotNull symbol
            && symbol.Assembly.SimpleName.Equals("FSharp.Core")

        let declaredElementsHaveSameTypes (left: IReferenceExpr) (right: IReferenceExpr) =
            let left = left.Reference.GetFcsSymbol()
            let right = right.Reference.GetFcsSymbol()

            match left, right with
            | :? FSharpMemberOrFunctionOrValue as mvfLeft, (:? FSharpMemberOrFunctionOrValue as mvfRight) ->
                mvfLeft.FullType.Equals(mvfRight.FullType)
            | _ ->
                false

        /// Is type of an element optimized out of the box
        let isCoreEqualityIntrinsic (element: IReferenceExpr) =
            let symbol = element.Reference.GetFcsSymbol()
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as value ->
                let typeOfValue = getAbbreviatedType value.FullType
                match typeOfValue.TryQualifiedBaseName with
                | Some name ->
                    match name with
                    | "System.Boolean"
                    | "System.SByte"
                    | "System.Int16"
                    | "System.Int32"
                    | "System.Int64"
                    | "System.Byte"
                    | "System.UInt16"
                    | "System.UInt32"
                    | "System.UInt64"
                    | "System.Single"
                    | "System.Double"
                    | "System.Char"
                    | "System.IntPtr"
                    | "System.UIntPtr"
                    | "System.Decimal"
                    | "System.String" ->
                        true
                    | _ ->
                        false
                | _ ->
                    false
            | _ ->
                false

        let isCustomEqualityCandidate (element: IReferenceExpr) =

            let symbol = element.Reference.GetFcsSymbol()
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as value ->

                let isStruct (fsType: FSharpType) =
                    fsType.HasTypeDefinition
                    && not fsType.TypeDefinition.IsClass

                let isIEquatableOfSelf (fsType: FSharpType) =
                    let interfaces = fsType.AllInterfaces
                    let rec isIEquatableOfSelf index =
                        if index >= interfaces.Count then
                            false
                        else
                            let possibleEquatable = getAbbreviatedType interfaces[index]
                            match possibleEquatable.TryQualifiedBaseName with
                            | Some "System.IEquatable`1" ->
                                fsType.Equals(possibleEquatable.GenericArguments[0])
                            | _ ->
                                isIEquatableOfSelf (index + 1)

                    isIEquatableOfSelf 0

                let isSpecialCaseForCustomEquality (fsType: FSharpType) =
                    match fsType.TryQualifiedBaseName with
                    | Some name ->
                        match name with
                        | "Microsoft.FSharp.Collections.FSharpList`1"
                        | "System.Collections.Generic.List`1"
                        | "System.Collections.Generic.IList`1"
                        | "System.Collections.Generic.HashSet`1"
                        | "System.Collections.Generic.IEnumerable`1" ->
                            isIEquatableOfSelf fsType.GenericArguments[0]
                        | "System.Collections.Generic.Dictionary`2" ->
                            isIEquatableOfSelf fsType.GenericArguments[0]
                            && isIEquatableOfSelf fsType.GenericArguments[1]
                        | _ ->
                            false
                    | _ ->
                        fsType.HasTypeDefinition
                        && match fsType.TypeDefinition.CompiledName with
                           | "[]`1" ->
                               isIEquatableOfSelf fsType.GenericArguments[0]
                           | _ ->
                               false

                let typeOfValue = getAbbreviatedType value.FullType

                (isStruct typeOfValue
                 && isIEquatableOfSelf typeOfValue)
                || isSpecialCaseForCustomEquality typeOfValue

            | _ ->
                false

        if isNotNull binaryAppExpr.Operator
           && isFsharpCoreEqualsOperator binaryAppExpr.Operator then
            match binaryAppExpr.LeftArgument, binaryAppExpr.RightArgument with
            | :? IReferenceExpr as leftArg, (:? IReferenceExpr as rightArg) ->
                if declaredElementsHaveSameTypes leftArg rightArg
                   && isCoreEqualityIntrinsic leftArg
                   && isCustomEqualityCandidate leftArg then
                    consumer.AddHighlighting(EqualityOperatorUsageWarning(binaryAppExpr))
                else
                    ()
            | _ ->
                ()

    interface IFSharpEqualityOperatorBoxingAnalyzer