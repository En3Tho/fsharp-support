namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Intentions

open FSharp.Compiler.Symbols
open JetBrains.Application.Settings
open JetBrains.Diagnostics
open JetBrains.ReSharper.Feature.Services.ContextActions
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Services.Formatter
open JetBrains.ReSharper.Psi.ExtensionsAPI
open JetBrains.ReSharper.Psi.ExtensionsAPI.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Resources.Shell

module PatUtil =
    // TODO: there should be an extension for this
    let tryParentOrReturnSelf<'a when 'a :> IFSharpPattern> (value: IFSharpPattern) =
        match value.Parent with
        | :? 'a as resultPat -> resultPat :> IFSharpPattern
        | _ -> value

    let getReturnTypeInfo (localRef: ILocalReferencePat) =
        // TODO: there should be an extension for this
        let rec tryNextSibling (ref: ITreeNode) =
            match ref.NextSibling with
            | null ->
                ValueNone
            | :? IReturnTypeInfo as returnTypeInfo ->
                ValueSome returnTypeInfo
            | sibling ->
                tryNextSibling sibling

        tryNextSibling localRef

    [<return: Struct>]
    let (|IsPartiallyAnnotatedNamedTypeUsage|_|) (namedTypeUsage: INamedTypeUsage) =
        let anonUsageExists =
            isNotNull namedTypeUsage.ReferenceName
            && isNotNull namedTypeUsage.ReferenceName.TypeArgumentList
            && namedTypeUsage.ReferenceName.TypeArgumentList.TypeUsagesEnumerable
               |> Seq.exists (fun typeUsage -> typeUsage :? IAnonTypeUsage)
        if anonUsageExists then
            ValueSome()
        else
            ValueNone

    [<return: Struct>]
    let (|IsPartiallyAnnotatedReturnTypeInfo|_|) (typedPat: IReturnTypeInfo) =
        match typedPat.ReturnType with
        | :? INamedTypeUsage as IsPartiallyAnnotatedNamedTypeUsage ->
            ValueSome()
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|IsPartiallyAnnotatedTypedPat|_|) (typedPat: ITypedPat) =
        match typedPat.TypeUsage with
        | :? INamedTypeUsage as IsPartiallyAnnotatedNamedTypeUsage ->
            ValueSome()
        | :? IFunctionTypeUsage as functionTypeUsage ->
            match functionTypeUsage.ArgumentTypeUsage, functionTypeUsage.ReturnTypeUsage with
            | :? INamedTypeUsage as IsPartiallyAnnotatedNamedTypeUsage, _
            | _, (:? INamedTypeUsage as IsPartiallyAnnotatedNamedTypeUsage) ->
                ValueSome()
            | _ ->
                ValueNone
        | _ ->
            ValueNone

    [<return: Struct>]
    let (|IsPartiallyAnnotatedLocalRefPat|_|) (localReference: ILocalReferencePat) =
        match getReturnTypeInfo localReference with
        | ValueSome IsPartiallyAnnotatedReturnTypeInfo ->
            ValueSome()
        | ValueNone ->
            ValueSome()
        | _ ->
            ValueNone

    let rec private isPartiallyAnnotatedTuplePat (localReference: ITuplePat) =
        localReference.Patterns
        |> Seq.exists (fun pattern ->
            match pattern with
            | :? ILocalReferencePat as IsPartiallyAnnotatedLocalRefPat ->
             true
            | :? ITuplePat as tuplePat ->
                isPartiallyAnnotatedTuplePat tuplePat
            | _ ->
                false)

    [<return: Struct>]
    let (|IsPartiallyAnnotatedTuplePat|_|) (localReference: ITuplePat) =
        if isPartiallyAnnotatedTuplePat localReference then
            ValueSome()
        else
            ValueNone

module SpecifyTypes =

    let specifyMethodReturnType displayContext (mfv: FSharpMemberOrFunctionOrValue) (method: IMemberDeclaration) =
        let typeString = mfv.ReturnParameter.Type.Format(displayContext)
        let anchor = method.ParametersDeclarations.Last()

        let factory = method.CreateElementFactory()
        let typeUsage = factory.CreateTypeUsage(typeString)

        let returnTypeInfo = ModificationUtil.AddChildAfter(anchor, factory.CreateReturnTypeInfo(typeUsage))

        let settingsStore = anchor.GetSettingsStoreWithEditorConfig()

        if settingsStore.GetValue(fun (key: FSharpFormatSettingsKey) -> key.SpaceBeforeColon) then
            ModificationUtil.AddChildBefore(returnTypeInfo, Whitespace()) |> ignore

    let specifyFunctionBindingReturnType displayContext (mfv: FSharpMemberOrFunctionOrValue) (binding: IBinding) =
        let typeString, anchor =
            let parameters = binding.ParametersDeclarations
            if parameters.Count > 0 then
                let rec skipFunctionParameters remaining (fullType: FSharpType) =
                    if remaining = 0 then fullType
                    else
                        skipFunctionParameters (remaining - 1) fullType.GenericArguments[1]

                let returnType = skipFunctionParameters parameters.Count mfv.FullType
                returnType.Format(displayContext), parameters.Last() :> ITreeNode
            else
                let headPat = binding.HeadPattern
                let headPatOrGenericParameterDeclaration =
                    match binding.HeadPattern.GetNextMeaningfulSibling() with
                    | :? IPostfixTypeParameterDeclarationList as typeParam ->
                        typeParam :> ITreeNode
                    | _ ->
                        headPat

                mfv.FullType.Format(displayContext), headPatOrGenericParameterDeclaration

        let factory = binding.CreateElementFactory()
        let typeUsage = factory.CreateTypeUsage(typeString)

        let returnTypeInfo = ModificationUtil.AddChildAfter(anchor, factory.CreateReturnTypeInfo(typeUsage))

        let settingsStore = anchor.GetSettingsStoreWithEditorConfig()

        // Unconditionally add space before ">" token or else compilation will fail
        if anchor :? IPostfixTypeParameterDeclarationList
           || settingsStore.GetValue(fun (key: FSharpFormatSettingsKey) -> key.SpaceBeforeColon) then
            ModificationUtil.AddChildBefore(returnTypeInfo, Whitespace()) |> ignore

    let private addParens (factory: IFSharpElementFactory) (pattern: IFSharpPattern) =
        let parenPat = factory.CreateParenPat()
        parenPat.SetPattern(pattern) |> ignore
        parenPat :> IFSharpPattern

    let private specifyValueType displayContext (fcsType: FSharpType) needToAddParens (pattern: IFSharpPattern) =
        let pattern = pattern.IgnoreParentParens()
        let factory = pattern.CreateElementFactory()

        let newPattern =
            match pattern.IgnoreInnerParens() with
            | :? ITypedPat as partiallyTypedPat ->
                // If it's a typed pat then it already has return type as a child
                partiallyTypedPat.Pattern

            | :? ILocalReferencePat as localRef ->
                // It's not a typed pat so it might have it's partial return type as a IReturnTypeInfo
                let rec removeReturnTypeInfo (ref: ITreeNode) =
                    match ref.NextSibling with
                    | null ->
                        ()
                    | :? IReturnTypeInfo as returnTypeInfo ->
                        ModificationUtil.DeleteChild(returnTypeInfo)
                    | sibling ->
                        removeReturnTypeInfo sibling

                removeReturnTypeInfo localRef
                localRef

            | pattern ->
                pattern

        let typedPat =
            let typedPat = factory.CreateTypedPat(newPattern, factory.CreateTypeUsage(fcsType.Format(displayContext)))
            if needToAddParens then
                addParens factory typedPat
            else
                typedPat :> _

        ModificationUtil.ReplaceChild(pattern, typedPat) |> ignore

    let specifyParameterType displayContext (fcsType: FSharpType) (pattern: IFSharpPattern) =
        specifyValueType displayContext fcsType true pattern

    let specifyLocalValueType displayContext (fcsType: FSharpType) (pattern: IFSharpPattern) =
        specifyValueType displayContext fcsType false pattern

    let specifyReferencePat (refPat: IReferencePat) =

        let symbolUse = refPat.GetFcsSymbolUse()
        if isNull symbolUse then () else

        let mfv = symbolUse.Symbol :?> FSharpMemberOrFunctionOrValue
        let displayContext = symbolUse.DisplayContext

        let patternToChange =
            refPat
            |> PatUtil.tryParentOrReturnSelf<ITypedPat>

        if refPat.Parent :? ITuplePat then
            specifyParameterType displayContext mfv.FullType patternToChange
        else
            match refPat.Binding with
            | :? IBinding as localBinding ->
                match localBinding.BindingKeyword.GetText() with
                | "let!" | "and!" | "use!" ->
                    specifyParameterType displayContext mfv.FullType patternToChange
                | _ ->
                    specifyLocalValueType displayContext mfv.FullType patternToChange
            | _ ->
                specifyParameterType displayContext mfv.FullType patternToChange

    let private getTupleChildrenRefPatsReferences (pattern: ITuplePat) =
        [|
            for pattern in pattern.Patterns do
                match pattern.IgnoreInnerParens() with
                | :? ILocalReferencePat as localRef ->
                    let symbol = localRef.GetFcsSymbolUse()
                    if isNotNull symbol then
                        struct (pattern, symbol)

                | :? ITypedPat as typedPat ->
                    match typedPat.Pattern with
                    | :? ILocalReferencePat as localRef ->
                        let symbol = localRef.GetFcsSymbolUse()
                        if isNotNull symbol then
                            struct (pattern, symbol)
                    | _ ->
                        ()

                | _ ->
                    ()
        |]

    let rec specifyTupleType (pattern: ITuplePat) =

        let referenceList = ResizeArray(pattern.Patterns.Count)

        let rec getRefPats (pattern: ITuplePat) =
            let childrenReferences = getTupleChildrenRefPatsReferences pattern
            referenceList.Add(struct (pattern, childrenReferences))
            for pattern in pattern.Patterns do
                match pattern.IgnoreInnerParens() with
                | :? ITuplePat as nestedTuplePat ->
                    getRefPats nestedTuplePat
                | _ ->
                    ()

        getRefPats pattern

        for i = referenceList.Count - 1 downto 0 do
            let struct (tuplePat, refPats) = referenceList[i]
            for refPat, symbolUse in refPats do
                let mfv = symbolUse.Symbol :?> FSharpMemberOrFunctionOrValue
                let displayContext = symbolUse.DisplayContext
                specifyLocalValueType displayContext mfv.FullType refPat

            match tuplePat.IgnoreParentParens() with
            | :? IParenPat ->
                ()
            | _ ->
                let factory = tuplePat.CreateElementFactory()
                let tuplePatternWithPats = addParens factory tuplePat
                ModificationUtil.ReplaceChild(tuplePat, tuplePatternWithPats) |> ignore

    let specifyPropertyType displayContext (fcsType: FSharpType) (decl: IMemberDeclaration) =
        Assertion.Assert(isNull decl.ReturnTypeInfo, "isNull decl.ReturnTypeInfo")
        Assertion.Assert(decl.ParametersDeclarationsEnumerable.IsEmpty(),
            "decl.ParametersDeclarationsEnumerable.IsEmpty()")

        let factory = decl.CreateElementFactory()
        let returnTypeInfo = factory.CreateReturnTypeInfo(factory.CreateTypeUsage(fcsType.Format(displayContext)))
        ModificationUtil.AddChildAfter(decl.Identifier, returnTypeInfo) |> ignore

    let specifyParameterDeclaration (parameter: IParametersPatternDeclaration) =
        match parameter.Pattern.IgnoreInnerParens() with
        | :? ITypedPat as (partiallyTypedPat & PatUtil.IsPartiallyAnnotatedTypedPat) ->
            match partiallyTypedPat.Pattern.IgnoreInnerParens() with
            | :? ILocalReferencePat as localRef ->
                specifyReferencePat localRef
            | _ ->
                ()

        | :? ILocalReferencePat as localRef ->
                specifyReferencePat localRef

        | :? ITuplePat as tuplePat ->
            specifyTupleType tuplePat

        | _ ->
            ()

[<ContextAction(Name = "AnnotateFunction", Group = "F#",
                Description = "Annotate function with parameter types and return type")>]
type FunctionAnnotationAction(dataProvider: FSharpContextActionDataProvider) =
    inherit FSharpContextActionBase(dataProvider)

    let isAnnotated (binding: IBinding) =
        isNotNull binding.ReturnTypeInfo &&
        binding.ParametersDeclarations |> Seq.forall (fun p ->
            let pattern = p.Pattern.IgnoreInnerParens()
            match pattern with
            | :? ITypedPat as PatUtil.IsPartiallyAnnotatedTypedPat -> false
            | :? ITypedPat | :? IUnitPat -> true
            | _ -> false)

    override x.Text = "Add function type annotations"

    override x.IsAvailable _ =
        let letBindings = dataProvider.GetSelectedElement<ILetBindings>() // check for IBinding Instead?
        if isNull letBindings then false else

        let bindings = letBindings.Bindings
        if bindings.Count <> 1 then false else

        let binding = bindings[0] // TODO: let .. and!
        (binding.ParametersDeclarations.Count > 0 || not (binding.HeadPattern :? ILocalReferencePat))
        && isAtLetExprKeywordOrReferencePattern dataProvider letBindings && not (isAnnotated binding)

    override x.ExecutePsiTransaction _ =
        let letBindings = dataProvider.GetSelectedElement<ILetBindings>() // IBinding?
        let binding = letBindings.Bindings |> Seq.exactlyOne

        use writeCookie = WriteLockCookie.Create(binding.IsPhysical())
        use disableFormatter = new DisableCodeFormatter()

        let refPat = binding.HeadPattern.As<IReferencePat>()
        if isNull refPat then () else

        let symbolUse = refPat.GetFcsSymbolUse()
        if isNull symbolUse then () else

        let mfv = symbolUse.Symbol :?> FSharpMemberOrFunctionOrValue
        let displayContext = symbolUse.DisplayContext

        binding.ParametersDeclarations |> Seq.iter SpecifyTypes.specifyParameterDeclaration

        if isNull binding.ReturnTypeInfo then
            SpecifyTypes.specifyFunctionBindingReturnType displayContext mfv binding