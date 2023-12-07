module JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.FSharpIdentifierTooltipProviderExtensions

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open JetBrains.ReSharper.Plugins.FSharp.Checker
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Util.FcsTaggedText
open JetBrains.ReSharper.Plugins.FSharp.Util.FSharpSymbolUtil
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.Modules

let (|Literal|_|) (symbolUse: FSharpSymbolUse) =
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        mfv.LiteralValue
    | :? FSharpField as field ->
        field.LiteralValue
    | _ ->
        None

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

let getDeclaringEntityHeaderIfExtensionMember(symbolUse: FSharpSymbolUse) = [
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

let getAbbreviatedEntityHeaderIfAbbreviation(symbolUse: FSharpSymbolUse) = [

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