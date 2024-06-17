module JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.IntrinsicsTable

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Compiler.Text
open JetBrains.ReSharper.Plugins.FSharp.Util.FcsTaggedText

type [<Struct>] IntrinsicInfo = {
    Description: ReadOnlyMemory<char>
    Operation: ReadOnlyMemory<char>
}

let parseIntrinsicsFile (text: string) =
    let table = Dictionary()
    let newLineLength = Environment.NewLine.Length

    let mutable textIndex = 0
    while textIndex < text.Length do
        let mutable mnemonicLengthIndex = textIndex
        while mnemonicLengthIndex < text.Length && Char.IsDigit(text[mnemonicLengthIndex]) do mnemonicLengthIndex <- mnemonicLengthIndex + 1
        let mnemonicLength = int text[textIndex..mnemonicLengthIndex]

        let mutable descriptionLengthIndex = mnemonicLengthIndex + 1
        while descriptionLengthIndex < text.Length && Char.IsDigit(text[descriptionLengthIndex]) do descriptionLengthIndex <- descriptionLengthIndex + 1
        let descriptionLength = int text[mnemonicLengthIndex + 1..descriptionLengthIndex]

        let mutable operationLengthIndex = descriptionLengthIndex + 1
        while operationLengthIndex < text.Length && Char.IsDigit(text[operationLengthIndex]) do operationLengthIndex <- operationLengthIndex + 1
        let operationLength = int text[descriptionLengthIndex + 1..operationLengthIndex]

        let descriptionStart = operationLengthIndex + 1 + mnemonicLength + newLineLength
        let operationStart = descriptionStart + descriptionLength + newLineLength

        textIndex <- operationStart + operationLength + newLineLength

        let mnemonic = text[operationLengthIndex + 1..operationLengthIndex + mnemonicLength]
        let description = text.AsMemory(descriptionStart, descriptionLength)
        let operation = text.AsMemory(operationStart, operationLength)

        table[mnemonic] <- { Description = description; Operation = operation }

    table

let readIntrinsicsTableFromEmbeddedResource() =
    let assembly = Assembly.GetExecutingAssembly()
    let resourceName = "JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Resources.Intrinsics.txt"
    use stream = assembly.GetManifestResourceStream(resourceName)
    use reader = new StreamReader(stream)
    let text = reader.ReadToEnd()
    parseIntrinsicsFile text

let private intrinsicsTable = readIntrinsicsTableFromEmbeddedResource()

let getIntrinsicInfo mnemonic =
    match intrinsicsTable.TryGetValue mnemonic with
    | true, info ->
        richText [|
            TaggedText(TextTag.Text, info.Description.ToString())
            TaggedText(TextTag.Text, Environment.NewLine)
            TaggedText(TextTag.Text, info.Operation.ToString())
        |] |> Some
    | _ -> None
