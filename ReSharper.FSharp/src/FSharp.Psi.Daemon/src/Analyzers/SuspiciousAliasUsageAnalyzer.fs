namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util

type IFSharpProperAliasUsageAnalyzer =
    inherit IElementProblemAnalyzer

module ProperAliasUsageAnalyzerUtil =
    
    let getSymbolUseOrSymbolReference (referenceOwner: IFSharpReferenceOwner) =
        if isNotNull referenceOwner.Reference then
            let symbol = referenceOwner.Reference.GetFcsSymbol()
            if isNotNull symbol then
                ValueSome symbol
            else
                let declaration = referenceOwner.FSharpFile.GetSymbolDeclaration(referenceOwner.Reference.SymbolOffset.Offset)
                if isNotNull declaration then
                    ValueSome declaration.Symbol
                else
                    ValueNone
        else
            ValueNone

    let getSymbolUse (referenceOwner: IFSharpReferenceOwner) =
        if isNotNull referenceOwner.Reference then
            let symbol = referenceOwner.Reference.GetFcsSymbol()
            if isNotNull symbol then
                ValueSome symbol
            else
                ValueNone
        else
            ValueNone

    [<return: Struct>]
    let (|HasFcsMfvSymbolUse|_|) (referenceOwner: IFSharpReferenceOwner) =
        match getSymbolUse referenceOwner with
        | ValueSome (:? FSharpMemberOrFunctionOrValue as mvf) ->
            ValueSome mvf
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|HasFcsMfvSymbolUseOrReference|_|) (referenceOwner: IFSharpReferenceOwner) =
        match getSymbolUseOrSymbolReference referenceOwner with
        | ValueSome (:? FSharpMemberOrFunctionOrValue as mvf) ->
            ValueSome mvf
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|HasFcsFSharpFieldSymbolUse|_|) (referenceOwner: IFSharpReferenceOwner) =
        match getSymbolUse referenceOwner with
        | ValueSome (:? FSharpField as mvf) ->
            ValueSome mvf
        | _ ->
            ValueNone
    
    let isFSharpNativeAbbreviation (abbreviationType: FSharpType) =
        match abbreviationType.TryQualifiedBaseName with
        | Some qualifiedBaseName ->
            match qualifiedBaseName with
            | "Microsoft.FSharp.Core.obj"
            | "Microsoft.FSharp.Core.exn"
            | "Microsoft.FSharp.Core.nativeint"
            | "Microsoft.FSharp.Core.unativeint"
            | "Microsoft.FSharp.Core.string"
            | "Microsoft.FSharp.Core.float32"
            | "Microsoft.FSharp.Core.float"
            | "Microsoft.FSharp.Core.single"
            | "Microsoft.FSharp.Core.double"
            | "Microsoft.FSharp.Core.sbyte"
            | "Microsoft.FSharp.Core.byte"
            | "Microsoft.FSharp.Core.int8"
            | "Microsoft.FSharp.Core.uint8"
            | "Microsoft.FSharp.Core.int16"
            | "Microsoft.FSharp.Core.uint16"
            | "Microsoft.FSharp.Core.int32"
            | "Microsoft.FSharp.Core.uint32"
            | "Microsoft.FSharp.Core.int64"
            | "Microsoft.FSharp.Core.uint64"
            | "Microsoft.FSharp.Core.char"
            | "Microsoft.FSharp.Core.bool"
            | "Microsoft.FSharp.Core.decimal"
            | "Microsoft.FSharp.Core.int"
            | "Microsoft.FSharp.Core.uint" ->
                true
            | _ ->
                false
        | _ ->
            false
    
    let bothTypesAreAbbreviations (left: FSharpType) (right: FSharpType) =
        left.IsAbbreviation
        && right.IsAbbreviation

    let abbreviatedTypesAreEqual (left: FSharpType) (right: FSharpType) =
        let leftType = getAbbreviatedType left
        let rightType = getAbbreviatedType right
        leftType.Equals(rightType)
    
    let abbreviationsAreChildrenOfEachOther (left: FSharpType) (right: FSharpType) =
        left.QualifiedBaseName.Equals(right.AbbreviatedType.QualifiedBaseName)
        || right.QualifiedBaseName.Equals(left.AbbreviatedType.QualifiedBaseName)
        || isFSharpNativeAbbreviation left
        || isFSharpNativeAbbreviation right

    let abbreviationsAreFSharpCoreNativeTypes (left: FSharpType) (right: FSharpType) =
        isFSharpNativeAbbreviation left
        && isFSharpNativeAbbreviation right
    
    let abbreviationsAreDifferent (left: FSharpType) (right: FSharpType) =
        not (left.QualifiedBaseName.Equals(right.QualifiedBaseName))

    let checkForWarning (left: FSharpType) (right: FSharpType) =
        let left, right =
            if not left.IsFunctionType && right.IsFunctionType then
                left, right.GenericArguments[right.GenericArguments.Count - 1]
            else
                left, right

        bothTypesAreAbbreviations left right
        && abbreviatedTypesAreEqual left right
        && not (abbreviationsAreChildrenOfEachOther left right)
        && abbreviationsAreDifferent left right
        && not (abbreviationsAreFSharpCoreNativeTypes left right)

    let checkPrefixAppExpr (consumer: IHighlightingConsumer) (parentExpression: IPrefixAppExpr) =
        if parentExpression.Arguments.Count > 0 then
            match parentExpression.FunctionExpression with
            | :? IReferenceExpr as (HasFcsMfvSymbolUse funcMfv) ->
                let mutable index = 0

                let prefixAppExpr =
                    let rec go (prefixAppExpr: IPrefixAppExpr) =
                        match prefixAppExpr.Parent with
                        | :? IPrefixAppExpr as prefixAppExpr ->
                            go prefixAppExpr
                        | _ ->
                            prefixAppExpr

                    go parentExpression

                let args = prefixAppExpr.Arguments
                for group in funcMfv.CurriedParameterGroups do
                    for parameter in group do
                        if index < args.Count then
                            let arg =  prefixAppExpr.Arguments[index]

                            match arg.Expression with
                            | :? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse argumentMfv) ->
                                if checkForWarning parameter.Type argumentMfv.FullType then
                                    consumer.AddHighlighting(SuspiciousAliasUsageWarning(wrongAliasRef))

                            | :? IPrefixAppExpr as localPrefixAppExpr ->
                                match localPrefixAppExpr.FunctionExpression with
                                | :? IReferenceExpr as (HasFcsMfvSymbolUse argumentMfv) ->
                                    if checkForWarning parameter.Type argumentMfv.FullType then
                                        consumer.AddHighlighting(SuspiciousAliasUsageWarning(localPrefixAppExpr))
                                | _ ->
                                    ()
                            | _ ->
                                ()
                            index <- index + 1
            | _ -> ()

    // x <- value
    let checkSetExpr (consumer: IHighlightingConsumer) (parentExpression: ISetExpr) =
        match parentExpression.LeftExpression, parentExpression.RightExpression with
        | :? IReferenceExpr as HasFcsMfvSymbolUse leftMfv, (:? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse rightMfv)) ->
           if checkForWarning leftMfv.FullType rightMfv.FullType then
                consumer.AddHighlighting(SuspiciousAliasUsageWarning(wrongAliasRef))
        | _ ->
            ()

    // let x = { A = ... }
    let checkRecordFieldBinding (consumer: IHighlightingConsumer) (parentExpression: IRecordFieldBinding) =
        match parentExpression.ReferenceName, parentExpression.LastChild with
        | HasFcsFSharpFieldSymbolUse recordField, (:? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse valueMfv)) ->
            if checkForWarning recordField.FieldType valueMfv.FullType then
                consumer.AddHighlighting(SuspiciousAliasUsageWarning(wrongAliasRef))
        | _ ->
            ()

    // let f x y = ...
    let checkLetOrUseExpr (consumer: IHighlightingConsumer) (parentExpression: ILetOrUseExpr) =
        for binding in parentExpression.Bindings do
            match binding.HeadPattern with
            | :? ILocalReferencePat as localPat ->
                match localPat.ReferenceName, binding.Expression with
                | HasFcsMfvSymbolUse localPatMfv, (:? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse valueMfv)) ->
                    if checkForWarning localPatMfv.FullType valueMfv.FullType then
                        consumer.AddHighlighting(SuspiciousAliasUsageWarning(wrongAliasRef))
                | _ ->
                    ()
            | _ ->
                ()

    // let x = y in module
    let checkChameleonExpr (consumer: IHighlightingConsumer) (originalExpression: IReferenceExpr) (parentExpression: IChameleonExpression) =
        match parentExpression.Parent with
        | :? ITopBinding as topBinding ->
            match topBinding.HeadPattern with
            | :? ITopReferencePat as topReference ->
                match topReference.ReferenceName, originalExpression with
                | HasFcsMfvSymbolUseOrReference topSymbol, (wrongAliasRef & HasFcsMfvSymbolUse valueMfv) ->
                    if checkForWarning topSymbol.FullType valueMfv.FullType then
                        consumer.AddHighlighting(SuspiciousAliasUsageWarning(wrongAliasRef))
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()

    // let x = y in local function
    let checkLocalBinding (consumer: IHighlightingConsumer) (originalExpression: IReferenceExpr) (parentExpression: ILocalBinding) =
        match parentExpression.HeadPattern with
        | :? ILocalReferencePat as localReference ->
            match localReference.ReferenceName, originalExpression with
            | HasFcsMfvSymbolUseOrReference topSymbol, (wrongAliasRef & HasFcsMfvSymbolUse valueMfv) ->
                if checkForWarning topSymbol.FullType valueMfv.FullType then
                    consumer.AddHighlighting(SuspiciousAliasUsageWarning(wrongAliasRef))
            | _ ->
                ()
        | _ ->
            ()
    
    // operators
    let checkBinaryAppExpr (consumer: IHighlightingConsumer) (parentExpression: IBinaryAppExpr) =
        match parentExpression.FunctionExpression with
        | :? IReferenceExpr as opExpr ->
            if opExpr.QualifiedName.Equals("|>") then
                match parentExpression.LeftArgument, parentExpression.RightArgument with
                // value |> f x
                | :? IReferenceExpr as (leftReferenceExpr & HasFcsMfvSymbolUse leftReferenceMfv), (:? IPrefixAppExpr as rightPrefixAppExpr) ->
                    match rightPrefixAppExpr.FunctionExpression with
                    | :? IReferenceExpr as (HasFcsMfvSymbolUse rightFuncMfv) ->
                        let funcType = rightFuncMfv.FullType
                        if funcType.IsFunctionType then // TODO: check for my custom pipe with delegate?
                            let argIndexToCheck = funcType.GenericArguments.Count - rightPrefixAppExpr.Arguments.Count
                            if argIndexToCheck < funcType.GenericArguments.Count then
                                // inner type is usually a func too, meaning a curried representation
                                let innerType = funcType.GenericArguments[argIndexToCheck]
                                let typeToCheck =
                                    if innerType.IsFunctionType then
                                        innerType.GenericArguments[0] // the next arg in curried representation of a func
                                    else
                                        innerType
                                if checkForWarning leftReferenceMfv.FullType typeToCheck then
                                    consumer.AddHighlighting(SuspiciousAliasUsageWarning(leftReferenceExpr))
                    | _ ->
                        ()
                // value |> f
                | :? IReferenceExpr as (leftReferenceExpr & HasFcsMfvSymbolUse leftReferenceMfv), (:? IReferenceExpr as (HasFcsMfvSymbolUse rightReferenceMfv)) ->
                    let funcType = rightReferenceMfv.FullType
                    if funcType.IsFunctionType then
                        if checkForWarning leftReferenceMfv.FullType funcType then
                            consumer.AddHighlighting(SuspiciousAliasUsageWarning(leftReferenceExpr))
                | _ ->
                    ()
        | _ ->
            ()

open ProperAliasUsageAnalyzerUtil

[<ElementProblemAnalyzer(typeof<ISetExpr>, HighlightingTypes = [| |])>]
type ISetExprAnalyzer() =
    inherit ElementProblemAnalyzer<ISetExpr>()
    override this.Run(element, _, consumer) = checkSetExpr consumer element
    interface IFSharpProperAliasUsageAnalyzer

[<ElementProblemAnalyzer(typeof<IPrefixAppExpr>, HighlightingTypes = [| |])>]
type IPrefixAppExprAnalyzer() =
    inherit ElementProblemAnalyzer<IPrefixAppExpr>()
    override this.Run(element, _, consumer) = checkPrefixAppExpr consumer element
    interface IFSharpProperAliasUsageAnalyzer

[<ElementProblemAnalyzer(typeof<IRecordFieldBinding>, HighlightingTypes = [| |])>]
type IRecordFieldBindingAnalyzer() =
    inherit ElementProblemAnalyzer<IRecordFieldBinding>()
    override this.Run(element, _, consumer) = checkRecordFieldBinding consumer element
    interface IFSharpProperAliasUsageAnalyzer

[<ElementProblemAnalyzer(typeof<ILetOrUseExpr>, HighlightingTypes = [| |])>]
type ILetOrUseExprAnalyzer() =
    inherit ElementProblemAnalyzer<ILetOrUseExpr>()
    override this.Run(element, _, consumer) = checkLetOrUseExpr consumer element
    interface IFSharpProperAliasUsageAnalyzer

[<ElementProblemAnalyzer(typeof<IBinaryAppExpr>, HighlightingTypes = [| |])>]
type IBinaryAppExprAnalyzer() =
    inherit ElementProblemAnalyzer<IBinaryAppExpr>()
    override this.Run(element, _, consumer) = checkBinaryAppExpr consumer element
    interface IFSharpProperAliasUsageAnalyzer

[<ElementProblemAnalyzer(typeof<IReferenceExpr>, HighlightingTypes = [| |])>]
type IReferenceExprAnalyzer() =
    inherit ElementProblemAnalyzer<IReferenceExpr>()

    override this.Run(originalReferenceExpression, _, consumer) =

        match originalReferenceExpression.Parent with
        | :? IChameleonExpression as chameleonExpr ->
            checkChameleonExpr consumer originalReferenceExpression chameleonExpr

        | :? ILocalBinding as localBinding ->
            checkLocalBinding consumer originalReferenceExpression localBinding

        | _ ->
            ()

    interface IFSharpProperAliasUsageAnalyzer