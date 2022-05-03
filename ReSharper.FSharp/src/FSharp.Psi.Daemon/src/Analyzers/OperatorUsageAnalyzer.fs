namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util

type IFSharpOperatorBoxingAnalyzer =
    inherit IElementProblemAnalyzer

module OperatorUsageAnalyzerUtil =

    type OperationType =
        | Equality
        | Comparison
        | NotSupported

    [<return: Struct>]
    let (|IsFromFSharpCore|_|) (mfv: FSharpMemberOrFunctionOrValue) =
        if mfv.Assembly.SimpleName.Equals("FSharp.Core") then
            ValueSome mfv
        else
            ValueNone

    [<return: Struct>]
    let (|HasMfvSymbol|_|) (referenceExpr: IReferenceExpr) =
        if isNull referenceExpr then ValueNone else
        match referenceExpr.Reference.GetFcsSymbol() with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
            ValueSome mfv
        | _ ->
            ValueNone

    let getOperationType(mfv: FSharpMemberOrFunctionOrValue) =
        match mfv.CompiledName with
        | "op_Equality"
        | "op_Inequality" ->
            OperationType.Equality
        | "op_GreaterThan"
        | "op_LessThan"
        | "op_GreaterThanOrEqual"
        | "op_LessThanOrEqual" ->
            OperationType.Comparison
        | _ ->
            OperationType.NotSupported

    let declaredElementsHaveSameTypes (left: FSharpMemberOrFunctionOrValue) (right: FSharpMemberOrFunctionOrValue) =
        left.FullType.Equals(right.FullType)

    /// Is type of an element optimized out of the box with native "=" operator
    let isCoreEqualityIntrinsic (value: FSharpMemberOrFunctionOrValue) =
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

    let isCoreComparisonIntrinsic (mfv: FSharpMemberOrFunctionOrValue) =
        let typeOfValue = getAbbreviatedType mfv.FullType
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

    let isStruct (fsType: FSharpType) =
        fsType.HasTypeDefinition
        && fsType.TypeDefinition.IsValueType

    let implementsInterfaceOfSelfType interfaceName (fsType: FSharpType) =
        let interfaces = fsType.AllInterfaces
        let rec implementsSelf index =
            if index >= interfaces.Count then
                false
            else
                let possibleEquatable = getAbbreviatedType interfaces[index]
                match possibleEquatable.TryQualifiedBaseName with
                | Some qualifiedBaseName when qualifiedBaseName.Equals interfaceName ->
                    fsType.Equals(possibleEquatable.GenericArguments[0])
                | _ ->
                    implementsSelf (index + 1)

        implementsSelf 0

    let isIEquatableOfSelf fsType =
        fsType |> implementsInterfaceOfSelfType "System.IEquatable`1"

    let isIComparableOfSelf fsType =
        fsType |> implementsInterfaceOfSelfType "System.IComparable`1"

    let isSpecialCaseForCustomEquality (fsType: FSharpType) =
        match fsType.TryQualifiedBaseName with
        | Some name ->
            match name with
            | "Microsoft.FSharp.Collections.FSharpList`1"
            | "System.Collections.Generic.List`1"
            | "System.Collections.Generic.LinkedList`1"
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

    let isCustomEqualityCandidate (value: FSharpMemberOrFunctionOrValue) =
        let typeOfValue = getAbbreviatedType value.FullType

        (isStruct typeOfValue
         && isIEquatableOfSelf typeOfValue)
        || isSpecialCaseForCustomEquality typeOfValue

    let isCustomComparisonCandidate (value: FSharpMemberOrFunctionOrValue) =
        let typeOfValue = getAbbreviatedType value.FullType

        (isStruct typeOfValue
         && isIComparableOfSelf typeOfValue)

    let checkBinaryAppExpr (consumer: IHighlightingConsumer) (binaryAppExpr: IBinaryAppExpr) =
        match binaryAppExpr.Operator, binaryAppExpr.LeftArgument, binaryAppExpr.RightArgument with
        | HasMfvSymbol opMfv, (:? IReferenceExpr as HasMfvSymbol leftArgMfv), (:? IReferenceExpr as HasMfvSymbol rightArgMfv) ->
            if declaredElementsHaveSameTypes leftArgMfv rightArgMfv then
                match getOperationType opMfv with
                | OperationType.NotSupported ->
                    ()
                | OperationType.Equality ->
                    if not (isCoreEqualityIntrinsic leftArgMfv)
                       && isCustomEqualityCandidate leftArgMfv then
                        consumer.AddHighlighting(EqualityOperatorUsageWarning(binaryAppExpr))
                    else
                        ()
                | OperationType.Comparison ->
                    if not (isCoreComparisonIntrinsic leftArgMfv)
                        && isCustomComparisonCandidate leftArgMfv then
                        consumer.AddHighlighting(EqualityOperatorUsageWarning(binaryAppExpr))
                    else
                        ()
        | _ ->
            ()
                
open OperatorUsageAnalyzerUtil

[<ElementProblemAnalyzer(typeof<IBinaryAppExpr>, HighlightingTypes = [| |])>]
type OperatorUsageAnalyzer() =
    inherit ElementProblemAnalyzer<IBinaryAppExpr>()
    
    override this.Run(binaryAppExpr, _, consumer) =
        checkBinaryAppExpr consumer binaryAppExpr

    interface IFSharpOperatorBoxingAnalyzer