[<AutoOpen; Extension>]
module JetBrains.ReSharper.Plugins.FSharp.Util.FSharpSymbolUtil

open System
open System.Collections.Generic
open FSharp.Compiler.Symbols
open JetBrains.Application.UI.Icons.ComposedIcons
open JetBrains.Diagnostics
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Psi.Resources
open JetBrains.UI.Icons
open JetBrains.Util.Logging

[<Extension; CompiledName("IsRefCell")>]
let isRefCell (mfv: FSharpMemberOrFunctionOrValue) =
    try mfv.IsRefCell
    with e ->
        Logger.LogMessage(LoggingLevel.WARN, "FSharpSymbolUtil.isRefCell fail: {0}", mfv)
        Logger.LogExceptionSilently(e)
        false

[<Extension; CompiledName("IsValCompiledAsMethod")>]
let isValCompiledAsMethod (mfv: FSharpMemberOrFunctionOrValue) =
    try mfv.IsValCompiledAsMethod
    with e ->
        Logger.LogMessage(LoggingLevel.WARN, "FSharpSymbolUtil.isValCompiledAsMethod fail: {0}", mfv)
        Logger.LogExceptionSilently(e)
        false

[<Extension; CompiledName("IsLiteral")>]
let isLiteral (mfv: FSharpMemberOrFunctionOrValue) =
    mfv.LiteralValue.IsSome

let isEnumMember (field: FSharpField) =
    match field.DeclaringEntity with
    | Some entity -> entity.IsEnum
    | _ -> false

[<CompiledName("GetReturnType")>]
let getReturnType (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv -> Some mfv.ReturnParameter.Type
    | :? FSharpField as field -> Some field.FieldType
    | :? FSharpParameter as param -> Some param.Type
    | :? FSharpUnionCase as case -> Some case.ReturnType
    | _ -> None

let addAccessibility (iconId: IconId) (accessibility: FSharpAccessibility) =
    if accessibility.IsPublic then iconId else
    if accessibility.IsInternal then compose iconId PsiSymbolsThemedIcons.ModifiersInternal.Id else
    if accessibility.IsPrivate then compose iconId PsiSymbolsThemedIcons.ModifiersPrivate.Id else
    compose iconId PsiSymbolsThemedIcons.ModifiersProtected.Id

let staticProperty = compose PsiSymbolsThemedIcons.Property.Id PsiSymbolsThemedIcons.ModifiersStatic.Id
let staticMethod   = compose PsiSymbolsThemedIcons.Method.Id   PsiSymbolsThemedIcons.ModifiersStatic.Id

let rec getIconId (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity ->
        if entity.IsNamespace then PsiSymbolsThemedIcons.Namespace.Id else

        let baseIcon =
            if entity.IsClass || entity.IsFSharpRecord
            then PsiSymbolsThemedIcons.Class.Id else

            if entity.IsFSharpModule then FSharpIcons.FSharpModule.Id else

            if entity.IsInterface then PsiSymbolsThemedIcons.Interface.Id else
            if entity.IsValueType then PsiSymbolsThemedIcons.Struct.Id else
            if entity.IsFSharpUnion || entity.IsEnum then PsiSymbolsThemedIcons.Enum.Id else
            if entity.IsDelegate then PsiSymbolsThemedIcons.Delegate.Id else

            let isTypeAbbr (entity: FSharpEntity) =
                entity.IsFSharpAbbreviation && entity.AbbreviatedType.HasTypeDefinition

            if not (isTypeAbbr entity) then PsiSymbolsThemedIcons.Class.Id else

            let mutable abbrEntity = entity
            while isTypeAbbr abbrEntity do
                abbrEntity <- abbrEntity.AbbreviatedType.TypeDefinition

            getIconId abbrEntity

        addAccessibility baseIcon entity.Accessibility

    | :? FSharpMemberOrFunctionOrValue as mfv ->
        let accessibility = mfv.Accessibility
        let isModuleValueOrMember = mfv.IsModuleValueOrMember
        let isMember = mfv.IsMember
        let isProperty = mfv.IsProperty
        let isCtor = mfv.IsConstructor

        if isModuleValueOrMember && not isMember && accessibility.IsPublic then
            if isValCompiledAsMethod mfv then staticMethod else staticProperty
        else

        let baseIcon =
            if not isModuleValueOrMember then PsiSymbolsThemedIcons.Variable.Id else

            if mfv.IsEvent then PsiSymbolsThemedIcons.Event.Id else
            if mfv.IsExtensionMember then PsiSymbolsThemedIcons.ExtensionMethod.Id else

            if isMember then
                if isProperty then PsiSymbolsThemedIcons.Property.Id else
                if isCtor then PsiSymbolsThemedIcons.Constructor.Id else
                PsiSymbolsThemedIcons.Method.Id

            else PsiSymbolsThemedIcons.Variable.Id

        let iconId =
            let readWriteModifier =
                if isProperty then
                    if mfv.HasSetterMethod then
                        if mfv.HasGetterMethod then PsiSymbolsThemedIcons.ModifiersReadWrite.Id else
                        PsiSymbolsThemedIcons.ModifiersWrite.Id
                    else
                        if mfv.HasGetterMethod then PsiSymbolsThemedIcons.ModifiersRead.Id else null

                else if mfv.IsMutable || isRefCell mfv then PsiSymbolsThemedIcons.ModifiersWrite.Id
                else null

            if isNull readWriteModifier then baseIcon
            else compose baseIcon readWriteModifier

        let iconId =
            if not isModuleValueOrMember || isCtor || mfv.IsInstanceMember then iconId
            else CompositeIconId.Compose(iconId, PsiSymbolsThemedIcons.ModifiersStatic.Id)

        addAccessibility iconId accessibility

    | :? FSharpField as field ->
        match field.DeclaringEntity with
        | Some entity when entity.IsEnum -> PsiSymbolsThemedIcons.EnumMember.Id
        | _ ->

        let iconId =
            let iconId = PsiSymbolsThemedIcons.Field.Id
            if field.IsStatic then compose iconId PsiSymbolsThemedIcons.ModifiersStatic.Id else iconId
        addAccessibility iconId field.Accessibility

    | :? FSharpUnionCase -> PsiSymbolsThemedIcons.EnumMember.Id
    | :? FSharpParameter -> PsiSymbolsThemedIcons.Parameter.Id
    | :? FSharpActivePatternCase -> PsiSymbolsThemedIcons.Method.Id
    | :? FSharpGenericParameter
    | :? FSharpStaticParameter -> PsiSymbolsThemedIcons.Typeparameter.Id
    | _ -> null


[<Extension; CompiledName("PatternName")>]
let patternName (pattern: FSharpActivePatternGroup) =
    match pattern.Name with
    | Some name -> name
    | _ ->

    let joinedNames = String.concat "|" pattern.Names
    let wildCase = if pattern.IsTotal then "|" else "_|"
    "|" + joinedNames + wildCase

[<Extension; CompiledName("GetAbbreviatedEntity")>]
let rec getAbbreviatedEntity (entity: FSharpEntity) =
    if entity.IsFSharpAbbreviation && entity.AbbreviatedType.HasTypeDefinition then
        getAbbreviatedEntity entity.AbbreviatedType.TypeDefinition
    else
        entity

let containsAttribute (attributeName: string) (attributeFullName: string) (attributes: IList<FSharpAttribute>) =
    if attributes = null || attributes.Count = 0 then false else
        let mutable i = 0
        let mutable found = false
        while not found && i < attributes.Count do
            let attribute = attributes[i].AttributeType
            i <- i + 1

            if attributeName.Equals(attribute.CompiledName, StringComparison.Ordinal) then
                match attribute.TryFullName with
                | Some fullName ->
                    found <- attributeFullName.Equals(fullName, StringComparison.Ordinal)
                | _ ->
                    ()
        found

[<Extension; CompiledName("ContainsMatchingSetterProperty")>]
let rec containsMatchingSetterProperty (entity: FSharpEntity, mfv: FSharpMemberOrFunctionOrValue) =
    let members = entity.MembersFunctionsAndValues
    let count = members.Count
    let mutable i = 0
    let mutable contains = false
    while not contains && i < count do
        let memberMfv = members[i]
        if (memberMfv.IsProperty && memberMfv.HasSetterMethod || memberMfv.IsPropertySetterMethod)
            && memberMfv.DisplayName.Equals(mfv.DisplayName, StringComparison.Ordinal) then
                contains <- (memberMfv.HasSetterMethod && memberMfv.SetterMethod.Accessibility.IsPublic) || (memberMfv.IsPropertySetterMethod && memberMfv.Accessibility.IsPublic)
                i <- count
        i <- i + 1
    contains

[<Extension; CompiledName("ContainsIsExternalInitAttribute")>]
let containsInitOnlyAttribute (attributes: IList<FSharpAttribute>) =
    containsAttribute (nameof(System.Runtime.CompilerServices.IsExternalInit)) "System.Runtime.CompilerServices.IsExternalInit" attributes

[<Extension; CompiledName("IsPropertyConstraint")>]
let isPropertyConstraintFSharpParameter (fsParam: FSharpParameter) =
    match fsParam.LogicalName with
    | ValueSome logicalName when fsParam.Type.IsFunctionType -> logicalName.StartsWith("get_", StringComparison.Ordinal)
    | _ -> false

[<Extension; CompiledName("IsOperatorConstraint")>]
let isOperatorConstraintFSharpParameter (fsParam: FSharpParameter) =
    match fsParam.LogicalName with
    | ValueSome logicalName when fsParam.Type.IsFunctionType -> logicalName.StartsWith("op_", StringComparison.Ordinal)
    | _ -> false

[<Extension; CompiledName("IsMethodConstraint")>]
let isMethodConstraintFSharpParameter (fsParam: FSharpParameter) =
    match fsParam.LogicalName with
    | ValueSome logicalName when fsParam.Type.IsFunctionType -> logicalName.StartsWith("get_", StringComparison.Ordinal) |> not
    | _ -> false

[<Extension; CompiledName("GetAbbreviatedType")>]
let rec getAbbreviatedType (fcsType: FSharpType) =
    if isNotNull fcsType && fcsType.IsAbbreviation then
        getAbbreviatedType fcsType.AbbreviatedType
    else
        fcsType

let tryGetAbbreviatedTypeEntity (fcsType: FSharpType) =
    let abbreviatedType = getAbbreviatedType fcsType
    if abbreviatedType.HasTypeDefinition then
        Some abbreviatedType.TypeDefinition
    else
        None

[<Extension; CompiledName("HasMeasureParameter")>]
let hasMeasureParameter(entity: FSharpEntity) =
    entity.GenericParameters.Count > 0 && entity.GenericParameters[0].IsMeasure

[<Extension; CompiledName("IsDisposable")>]
let rec isDisposableType (type': FSharpType) =

    let mutable isOwned = false
    let mutable isRented = false

    if type'.IsAbbreviation then
        match type'.TryBasicQualifiedName with
        | Some bqn when bqn.Equals("En3Tho.FSharp.Extensions.Core+Owned`1") -> isOwned <- true
        | Some bqn when bqn.Equals("En3Tho.FSharp.Extensions.Core+Rented`1") -> isRented <- true
        | _ -> ()

    if isRented then
        false
    else
        let typeToCheck = if isOwned then type'.GenericArguments[0] else type'
        typeToCheck.AllInterfaces |> Seq.exists (fun x ->
            match x.BasicQualifiedName with
            | bqn when bqn.Equals(typeof<IDisposable>.FullName, StringComparison.Ordinal) -> true
            | bqn when bqn.Equals(typeof<IAsyncDisposable>.FullName, StringComparison.Ordinal) -> true
            | _ -> false)

[<Extension; CompiledName("IsDisposable")>]
let isDisposableEntity (entity: FSharpEntity) =
    isDisposableType (entity.AsType())

[<Extension; CompiledName("IsException")>]
let isExceptionEntity (entity: FSharpEntity) =
    match entity.TryBasicQualifiedName with
    | Some nm when nm.Equals(typeof<Exception>.FullName, StringComparison.Ordinal) ->
        true
    | _ ->
        let rec isException (fcsType: FSharpType) =
            match fcsType.TryBasicQualifiedName with
            | Some nm when nm.Equals(typeof<Exception>.FullName, StringComparison.Ordinal) ->
                true
            | _ ->
                match fcsType.BaseType with
                | Some baseType -> baseType |> getAbbreviatedType |> isException
                | None -> false

        entity.BaseType
        |> Option.map getAbbreviatedType
        |> Option.map isException
        |> Option.defaultValue false

[<Extension; CompiledName("ContainsExtensionAttribute")>]
let containsExtensionAttribute (attributes: IList<FSharpAttribute>) =
    containsAttribute (nameof(System.Runtime.CompilerServices.ExtensionAttribute)) "System.Runtime.CompilerServices.ExtensionAttribute" attributes

[<Extension; CompiledName("IsExtensionMemberMfv")>]
let isExtensionMemberMfv (mfv: FSharpMemberOrFunctionOrValue) =
    mfv.IsExtensionMember || containsExtensionAttribute mfv.Attributes

[<Extension; CompiledName("IsInlineable")>]
let isInlineable (mfv: FSharpMemberOrFunctionOrValue) =
    not (
        mfv.InlineAnnotation.Equals(FSharpInlineAnnotation.NeverInline)
        || mfv.InlineAnnotation.Equals(FSharpInlineAnnotation.OptionalInline)
    )

type FSharpActivePatternGroup with
    member x.PatternName = patternName x


type FcsEntityInstance =
    { Entity: FSharpEntity
      Substitution: (FSharpGenericParameter * FSharpType) list }

    member this.FcsType =
        this.Entity.AsType().Instantiate(this.Substitution)

    override x.ToString() = x.Entity.ToString()

module FcsEntityInstance =
    let create fcsType =
        let fcsType = getAbbreviatedType fcsType
        if isNull fcsType || not fcsType.HasTypeDefinition then Unchecked.defaultof<_> else

        let fcsEntity = fcsType.TypeDefinition
        let substitution = Seq.zip fcsEntity.GenericParameters fcsType.GenericArguments |> Seq.toList

        { Entity = fcsEntity
          Substitution = substitution }


type FcsMfvInstance =
    { Mfv: FSharpMemberOrFunctionOrValue
      DisplayContext: FSharpDisplayContext
      Substitution: (FSharpGenericParameter * FSharpType) list }

    override x.ToString() = x.Mfv.ToString()

module FcsMfvInstance =
    let create mfv displayContext substitution =
        { Mfv = mfv
          Substitution = substitution
          DisplayContext = displayContext }
