namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util

type IFSharpProperAliasUsageAnalyzer =
    inherit IElementProblemAnalyzer

[<ElementProblemAnalyzer(typeof<IReferenceExpr>, HighlightingTypes = [| |])>]
type ProperAliasUsageAnalyzer() =
    inherit ElementProblemAnalyzer<IReferenceExpr>()

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

    let bothTypesAreAbbreviations (left: FSharpType) (right: FSharpType) =
            left.IsAbbreviation && right.IsAbbreviation

    let abbreviatedTypesAreEqual (left: FSharpType) (right: FSharpType) =
        let leftType = getAbbreviatedType left
        let rightType = getAbbreviatedType right
        leftType.Equals(rightType)

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
        && abbreviationsAreDifferent left right

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
                                    consumer.AddHighlighting(ProperAliasUsageWarning(wrongAliasRef))

                            | :? IPrefixAppExpr as localPrefixAppExpr ->
                                match localPrefixAppExpr.FunctionExpression with
                                | :? IReferenceExpr as (HasFcsMfvSymbolUse argumentMfv) ->
                                    if checkForWarning parameter.Type argumentMfv.FullType then
                                        consumer.AddHighlighting(ProperAliasUsageWarning(localPrefixAppExpr))
                                | _ ->
                                    ()
                            | _ ->
                                ()
                            index <- index + 1
            | _ -> ()

    let checkSetExpr (consumer: IHighlightingConsumer) (parentExpression: ISetExpr) =
        match parentExpression.LeftExpression, parentExpression.RightExpression with
        | :? IReferenceExpr as HasFcsMfvSymbolUse leftMfv, (:? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse rightMfv)) ->
           if checkForWarning leftMfv.FullType rightMfv.FullType then
                consumer.AddHighlighting(ProperAliasUsageWarning(wrongAliasRef))
        | _ ->
            ()

    let checkRecordFieldBinding (consumer: IHighlightingConsumer) (parentExpression: IRecordFieldBinding) =
        match parentExpression.ReferenceName, parentExpression.LastChild with
        | HasFcsFSharpFieldSymbolUse recordField, (:? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse valueMfv)) ->
            if checkForWarning recordField.FieldType valueMfv.FullType then
                consumer.AddHighlighting(ProperAliasUsageWarning(wrongAliasRef))
        | _ ->
            ()

    let checkLetOrUseExpr (consumer: IHighlightingConsumer) (parentExpression: ILetOrUseExpr) =
        for binding in parentExpression.Bindings do
            match binding.HeadPattern with
            | :? ILocalReferencePat as localPat ->
                match localPat.ReferenceName, binding.Expression with
                | HasFcsMfvSymbolUse localPatMfv, (:? IReferenceExpr as (wrongAliasRef & HasFcsMfvSymbolUse valueMfv)) ->
                    if checkForWarning localPatMfv.FullType valueMfv.FullType then
                        consumer.AddHighlighting(ProperAliasUsageWarning(wrongAliasRef))
                | _ ->
                    ()
            | _ ->
                ()

    let checkChameleonExpr (consumer: IHighlightingConsumer) (originalExpression: IReferenceExpr) (parentExpression: IChameleonExpression) =
        match parentExpression.Parent with
        | :? ITopBinding as topBinding ->
            match topBinding.HeadPattern with
            | :? ITopReferencePat as topReference ->
                match topReference.ReferenceName, originalExpression with
                | HasFcsMfvSymbolUseOrReference topSymbol, (wrongAliasRef & HasFcsMfvSymbolUse valueMfv) ->
                    if checkForWarning topSymbol.FullType valueMfv.FullType then
                        consumer.AddHighlighting(ProperAliasUsageWarning(wrongAliasRef))
                | _ ->
                    ()
            | _ ->
                ()
        | _ ->
            ()

    let checkLocalBinding (consumer: IHighlightingConsumer) (originalExpression: IReferenceExpr) (parentExpression: ILocalBinding) =
        match parentExpression.HeadPattern with
        | :? ILocalReferencePat as localReference ->
            match localReference.ReferenceName, originalExpression with
            | HasFcsMfvSymbolUseOrReference topSymbol, (wrongAliasRef & HasFcsMfvSymbolUse valueMfv) ->
                if checkForWarning topSymbol.FullType valueMfv.FullType then
                    consumer.AddHighlighting(ProperAliasUsageWarning(wrongAliasRef))
            | _ ->
                ()
        | _ ->
            ()

    let checkBinaryAppExpr (consumer: IHighlightingConsumer) (parentExpression: IBinaryAppExpr) =
        match parentExpression.FunctionExpression with
        | :? IReferenceExpr as opExpr ->
            if opExpr.QualifiedName.Equals("|>") then

                match parentExpression.LeftArgument, parentExpression.RightArgument with
                | :? IReferenceExpr as (leftReferenceExpr & HasFcsMfvSymbolUse leftReferenceMfv), (:? IPrefixAppExpr as rightPrefixAppExpr) ->
                    match rightPrefixAppExpr.FunctionExpression with
                    | :? IReferenceExpr as (HasFcsMfvSymbolUse rightFuncMfv) ->
                        let funcType = rightFuncMfv.FullType
                        if funcType.IsFunctionType then
                            let argIndexToCheck = funcType.GenericArguments.Count - rightPrefixAppExpr.Arguments.Count
                            if argIndexToCheck < funcType.GenericArguments.Count then
                                let innerType = funcType.GenericArguments[argIndexToCheck]
                                let typeToCheck =
                                    if innerType.IsFunctionType then
                                        innerType.GenericArguments[0] // the next arg in curried representation of a func?
                                    else
                                        innerType
                                if checkForWarning leftReferenceMfv.FullType typeToCheck then
                                    consumer.AddHighlighting(ProperAliasUsageWarning(leftReferenceExpr))
                    | _ ->
                        ()

                | :? IReferenceExpr as (leftReferenceExpr & HasFcsMfvSymbolUse leftReferenceMfv), (:? IReferenceExpr as (HasFcsMfvSymbolUse rightReferenceMfv)) ->
                    let funcType = rightReferenceMfv.FullType
                    if funcType.IsFunctionType then
                        if checkForWarning leftReferenceMfv.FullType funcType then
                            consumer.AddHighlighting(ProperAliasUsageWarning(leftReferenceExpr))
                | _ ->
                    ()
        | _ ->
            ()

    // TODO: Find a way not to report ReferenceExpressions in PrefixAppExpressions multiple times.
    // Maybe splitting an analyzer might help?

    override this.Run(originalReferenceExpression, data, consumer) =

        match originalReferenceExpression.Parent with
        | :? ISetExpr as setExpr ->
            checkSetExpr consumer setExpr

        | :? IPrefixAppExpr as prefixAppExpr ->
            checkPrefixAppExpr consumer prefixAppExpr

        | :? IRecordFieldBinding as recordFieldBinding ->
            checkRecordFieldBinding consumer recordFieldBinding

        | :? ILetOrUseExpr as letOrUseExpr ->
            checkLetOrUseExpr consumer letOrUseExpr

        | :? IChameleonExpression as chameleonExpr ->
            checkChameleonExpr consumer originalReferenceExpression chameleonExpr

        | :? ILocalBinding as localBinding ->
            checkLocalBinding consumer originalReferenceExpression localBinding

        | :? IBinaryAppExpr as binaryAppExpr ->
            checkBinaryAppExpr consumer binaryAppExpr

        | _ ->
            ()

    interface IFSharpProperAliasUsageAnalyzer