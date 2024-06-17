module JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.FSharpIdentifierTooltipProviderExtensions

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open JetBrains.ReSharper.Plugins.FSharp.Checker
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util.FcsTaggedText
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.Modules

type OptionCode<'T> = unit -> 'T voption

type OptionBuilderBase() =

    member inline _.Zero() : OptionCode<unit> =
        fun() -> ValueSome ()

    member inline _.Delay([<InlineIfLambda>] f: unit -> OptionCode<'T>) : OptionCode<'T> =
        fun () -> (f())()

    member inline _.Combine([<InlineIfLambda>] task1: OptionCode<unit>, [<InlineIfLambda>] task2: OptionCode<'T>) : OptionCode<'T> =
        fun () ->
            match task1() with
            | ValueNone -> ValueNone
            | ValueSome() -> task2()

    member inline _.Bind(res1: 'T1 option, [<InlineIfLambda>] task2: 'T1 -> OptionCode<'T>) : OptionCode<'T> =
        fun () ->
            match res1 with
            | None -> ValueNone
            | Some v -> (task2 v)()

    member inline _.Bind(res1: 'T1 voption, [<InlineIfLambda>] task2: 'T1 -> OptionCode<'T>) : OptionCode<'T> =
        fun () ->
            match res1 with
            | ValueNone -> ValueNone
            | ValueSome v -> (task2 v)()

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, [<InlineIfLambda>] body: OptionCode<unit>) : OptionCode<unit> =
        fun () ->
            let mutable proceed = true
            while proceed && condition() do
                match body() with
                | ValueNone -> proceed <- false
                | ValueSome () -> ()
            ValueSome(())

    member inline _.TryWith([<InlineIfLambda>] body: OptionCode<'T>, [<InlineIfLambda>] catch: exn -> OptionCode<'T>) : OptionCode<'T> =
        fun () ->
            try
                body()
            with exn ->
                (catch exn)()

    member inline _.TryFinally([<InlineIfLambda>] body: OptionCode<'T>, [<InlineIfLambda>] compensation: unit -> unit) : OptionCode<'T> =
        fun () ->
            try
                body()
            finally
                compensation()

    member inline this.Using(disp: #IDisposable, [<InlineIfLambda>] body: #IDisposable -> OptionCode<'T>) : OptionCode<'T> =
        this.TryFinally(
            (fun () -> (body disp)()),
            (fun () -> if not (isNull (box disp)) then disp.Dispose()))

    member inline this.For(sequence: seq<'TElement>, [<InlineIfLambda>] body : 'TElement -> OptionCode<unit>) : OptionCode<unit> =
        this.Using (
            sequence.GetEnumerator(),
            (fun e -> this.While((fun () -> e.MoveNext()),
            (fun () -> (body e.Current)()))))

    member inline _.Return (value: 'T) : OptionCode<'T> =
        fun () ->
            ValueSome value

    member inline this.ReturnFrom (source: option<'T>) : OptionCode<'T> =
        fun () ->
            match source with Some x -> ValueSome x | None -> ValueNone

    member inline this.ReturnFrom (source: voption<'T>) : OptionCode<'T> =
        fun () ->
            source

type OptionBuilder() =
    inherit OptionBuilderBase()

    member inline _.Run([<InlineIfLambda>] code: OptionCode<'T>) : 'T option =
         match code() with
         | ValueNone -> None
         | ValueSome v -> Some v

type ValueOptionBuilder() =
    inherit OptionBuilderBase()

    member inline _.Run([<InlineIfLambda>] code: OptionCode<'T>) : 'T voption =
        code()

let option = OptionBuilder()
let voption = ValueOptionBuilder()

let (|Literal|_|) (symbolUse: FSharpSymbolUse) =
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        mfv.LiteralValue
    | :? FSharpField as field ->
        field.LiteralValue
    | _ ->
        None

let [<Literal>] IntrinsicsNamespace = "System.Runtime.Intrinsics"
let (|RuntimeIntrinsic|_|) (symbolUse: FSharpSymbolUse) = option {
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        if mfv.IsMember && mfv.IsMethod then
            let! entity = mfv.DeclaringEntity
            let entityType = entity.AsType()
            let! qualifiedName = entityType.TryBasicQualifiedName
            if qualifiedName.StartsWith(IntrinsicsNamespace)
                && (let nextChar = qualifiedName[IntrinsicsNamespace.Length + 1] // + '.' + char
                    nextChar = 'X' || nextChar = 'A' || nextChar = 'W') then
                return mfv
            else
                return! None
        else
            return! None
    | _ ->
        return! None
}

let getToolTipTextForEntity(fsEntity: FSharpEntity, displayContext: FSharpDisplayContext, includeEntityType) =
    let entities = [
        let entities = [|

            if includeEntityType then
                fsEntity

            let mutable declaringEntityOpt = fsEntity.DeclaringEntity
            while declaringEntityOpt.IsSome do
                let declaringEntity = declaringEntityOpt.Value
                declaringEntityOpt <- declaringEntity.DeclaringEntity
                declaringEntity
        |]

        for i = entities.Length - 1 downto 0 do
            let entity = entities[i]
            let layout = entity.AsType().FormatLayout(displayContext)
            yield! layout

            if i > 0 then
                TaggedText(TextTag.Punctuation, ".")
    ]
    entities |> List.filter (fun text -> text.Tag <> TextTag.UnknownEntity)

let getDeclaringEntityIfExtensionMember(symbolUse: FSharpSymbolUse) = [
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        if mfv.IsExtensionMember then
            match mfv.DeclaringEntity with
            | Some entity ->
                let displayContext = symbolUse.DisplayContext
                let typeText = [|
                    TaggedText(TextTag.Text, "Extension from: ")
                    yield! getToolTipTextForEntity(entity, displayContext, true)
                |]
                richText typeText
            | None ->
                ()
    | _ ->
        ()
]

let getAbbreviatedEntityIfAbbreviation(symbolUse: FSharpSymbolUse) = [

    let getAbbreviatedEntityOnce(entity: FSharpEntity) =
        if entity.IsFSharpAbbreviation then
            let abbreviatedType = entity.AbbreviatedType
            if abbreviatedType.HasTypeDefinition then
                Some abbreviatedType.TypeDefinition
            else
                None
        else
            None

    let getAbbreviatedTypeOnce(ty: FSharpType) =
        if ty.IsAbbreviation then
            Some ty.AbbreviatedType
        else
            None

    match symbolUse.Symbol with
    | :? FSharpEntity as entity ->
        let mutable entity = entity
        let mutable go = true
        while go do
            match getAbbreviatedEntityOnce entity with
            | Some abbreviatedEntity ->
                match getAbbreviatedTypeOnce (entity.AsType()) with
                | Some abbreviatedType ->
                    let displayContext = symbolUse.DisplayContext
                    let typeText = [|
                        TaggedText(TextTag.Text, "Abbreviated type: ")
                        yield! getToolTipTextForEntity(abbreviatedEntity, displayContext, false)
                        yield! abbreviatedType.FormatLayout(displayContext)
                    |]
                    richText typeText
                    entity <- abbreviatedEntity
                | _ ->
                    go <- false
            | _ ->
                go <- false
    | _ ->
        ()
]

let getHexRepresentationIfIntConstant(symbolUse: FSharpSymbolUse) = [
    match symbolUse with
    | Literal literal ->
        match literal with
        | :? int8
        | :? int16
        | :? int32
        | :? int64
        | :? uint8
        | :? uint16
        | :? uint32
        | :? uint64
        | :? char ->
            let hexString = $"{literal:X}"
            let hexString = $"0x{hexString.PadLeft(hexString.Length + (hexString.Length &&& 1), '0')}"
            let typeText = [|
                TaggedText(TextTag.Text, "(")
                TaggedText(TextTag.NumericLiteral, hexString)
                TaggedText(TextTag.Text, ")")
            |]
            richText typeText
        | _ ->
            ()
    | _ ->
        ()
]

let getLengthIfStringConstant(symbolUse: FSharpSymbolUse) = [
    match symbolUse with
    | Literal (:? string as str) ->
        let length = str.Length
        let typeText = [|
            TaggedText(TextTag.Text, "(")
            TaggedText(TextTag.NumericLiteral, length.ToString())
            TaggedText(TextTag.Text, ")")
        |]
        richText typeText
    | _ ->
        ()
]

// this is what we're looking at
// <summary>
//   <para>__m256 _mm256_mul_ps (__m256 a, __m256 b)</para>
//   <para>VMULPS ymm, ymm, ymm/m256</para>
// </summary>
// <param name="left" />
// <param name="right" />
// public static Vector256<float> Multiply(Vector256<float> left, Vector256<float> right)
let getDescriptionIfRuntimeIntrinsic (xmlDocService: FSharpXmlDocService) (xmlDoc: FSharpXmlDoc) (symbolUse: FSharpSymbolUse) = [
    match symbolUse with
    | RuntimeIntrinsic _ ->
        match xmlDocService.GetXmlDocSummary2(xmlDoc) with
        | Some value ->
            let firstPara = value.SelectSingleNode("para")
            if firstPara <> null then
                let line = firstPara.InnerText
                let indexOfSpaceBeforeMnemonic = line.IndexOf(" ")
                if indexOfSpaceBeforeMnemonic > 0 then
                    let indexOfSpaceAfterMnemonic = line.IndexOf(" ", indexOfSpaceBeforeMnemonic + 1)
                    if indexOfSpaceAfterMnemonic > 0 then
                        let mnemonic = line.Substring(indexOfSpaceBeforeMnemonic + 1, indexOfSpaceAfterMnemonic - indexOfSpaceBeforeMnemonic - 1)
                        match IntrinsicsTable.getIntrinsicInfo mnemonic with
                        | Some description -> description
                        | _ -> ()
        | _ -> ()
    | _ ->
        ()
]

let getFSharpEntityByIdentifier (results: FSharpParseAndCheckResults) (token: IFSharpIdentifier) =
    // todo: fix getting qualifiers
    let tokenNames = [token.Name]

    let sourceFile = token.GetSourceFile()
    let coords = sourceFile.Document.GetCoordsByOffset(token.GetTreeEndOffset().Offset)
    let lineText = sourceFile.Document.GetLineText(coords.Line)
    use cookie = CompilationContextCookie.GetOrCreate(sourceFile.GetPsiModule().GetContextFromModule())

    // todo: provide tooltip for #r strings in fsx, should pass String tag
    results.CheckResults.GetSymbolUseAtLocation(int coords.Line + 1, int coords.Column, lineText, tokenNames)

let parseAndGetFSharpEntityByIdentifier(token: IFSharpIdentifier) =
    match token.FSharpFile.GetParseAndCheckResults(true, "FSharpQuickDoc") with
    | Some results ->
        getFSharpEntityByIdentifier results token
    | _ ->
        None
