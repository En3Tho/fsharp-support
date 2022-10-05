namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers

open System
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
    let (|HasMfvSymbol|_|) (referenceExpr: IFSharpReferenceOwner) =
        if isNull referenceExpr then ValueNone else
        match referenceExpr.Reference.GetFcsSymbol() with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
            ValueSome mfv
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|HasFSharpType|_|) (expr: IFSharpExpression) =
        match expr with
        | :? IFSharpReferenceOwner as HasMfvSymbol mfv ->
            ValueSome mfv.FullType
        | :? IFSharpReferenceOwner as refOwner ->
            ValueNone
        | :? IRecordExpr as recordExpr ->
            ValueNone
        | :? IListExpr as listExpr ->
            ValueNone
        | :? IArrayExpr as arrayExpr ->
            ValueNone
        | :? IComputationExpr as compExpr ->
            ValueNone
        | _ ->
            ValueNone

    let isFromFSharpCore(mfv: FSharpMemberOrFunctionOrValue) =
        "FSharp.Core".Equals(mfv.Assembly.SimpleName, StringComparison.Ordinal)

    let getOperationType(mfv: FSharpMemberOrFunctionOrValue) =
        // TODO: Check it's from FSharpCore actually
        match mfv.CompiledName with
        | "op_Equality"
        | "op_Inequality" when isFromFSharpCore mfv ->
            OperationType.Equality
        | "op_GreaterThan"
        | "op_LessThan"
        | "op_GreaterThanOrEqual"
        | "op_LessThanOrEqual" when isFromFSharpCore mfv ->
            OperationType.Comparison
        | _ ->
            OperationType.NotSupported

    let areSameTypes (left: FSharpType) (right: FSharpType) =
        left.Equals(right)

    /// Is type of an element optimized out of the box with native "=" operator
    let isCoreEqualityIntrinsic (value: FSharpType) =
        let typeOfValue = getAbbreviatedType value
        match typeOfValue.TryQualifiedBaseName with
        | Some name ->
            // GenericEqualityFast
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
            | "System.String"
            | "System.Decimal"
            | "System.DateTime" ->
                true
            | _ ->
                false
        | _ ->
            false

    let isCoreComparisonIntrinsic (value: FSharpType) =
        let typeOfValue = getAbbreviatedType value
        match typeOfValue.TryQualifiedBaseName with
        // GenericGreaterThanFast
        // Note String is not here
        | Some name ->
            match name with
            | "System.Boolean"
            | "System.SByte"
            | "System.Int16"
            | "System.Int32"
            | "System.Int64"
            | "System.IntPtr"
            | "System.Byte"
            | "System.UInt16"
            | "System.UInt32"
            | "System.UInt64"
            | "System.UIntPtr"
            | "System.Single"
            | "System.Double"
            | "System.Char"
            | "System.Decimal"
            | "System.DateTime" ->
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

    let isCustomEqualityCandidate (value: FSharpType) =
        let typeOfValue = getAbbreviatedType value

        (isStruct typeOfValue
         && isIEquatableOfSelf typeOfValue)
        || isSpecialCaseForCustomEquality typeOfValue

    let isCustomComparisonCandidate (value: FSharpType) =
        let typeOfValue = getAbbreviatedType value

        (isStruct typeOfValue
         && isIComparableOfSelf typeOfValue)

    let checkBinaryAppExpr (consumer: IHighlightingConsumer) (binaryAppExpr: IBinaryAppExpr) =
        // This might be not a reference expr, maybe try IReferenceOwner?
        // I need only Type, not reference?
        match binaryAppExpr.Operator, binaryAppExpr.LeftArgument, binaryAppExpr.RightArgument with
        | HasMfvSymbol opMfv, HasFSharpType leftType, HasFSharpType rightType ->
            
            if areSameTypes leftType rightType then
                match getOperationType opMfv with
                | OperationType.NotSupported ->
                    ()
                | OperationType.Equality ->
                    if not (isCoreEqualityIntrinsic leftType)
                       && isCustomEqualityCandidate leftType then
                        consumer.AddHighlighting(EqualityOperatorUsageWarning(binaryAppExpr))
                | OperationType.Comparison ->
                    if not (isCoreComparisonIntrinsic leftType)
                        && isCustomComparisonCandidate leftType then
                        consumer.AddHighlighting(EqualityOperatorUsageWarning(binaryAppExpr))
        | _ ->
            ()
                
open OperatorUsageAnalyzerUtil

[<ElementProblemAnalyzer(typeof<IBinaryAppExpr>, HighlightingTypes = [| |])>]
type OperatorUsageAnalyzer() =
    inherit ElementProblemAnalyzer<IBinaryAppExpr>()
    
    override this.Run(binaryAppExpr, _, consumer) =
        checkBinaryAppExpr consumer binaryAppExpr

    interface IFSharpOperatorBoxingAnalyzer