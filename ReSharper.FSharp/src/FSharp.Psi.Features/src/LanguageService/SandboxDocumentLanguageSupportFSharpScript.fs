namespace JetBrains.ReSharper.Plugins.FSharp.Psi.LanguageService

open System.Collections.Generic
open JetBrains.ProjectModel
open JetBrains.RdBackend.Common.Features.Documents
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Psi
open JetBrains.Rider.Model

[<Language(typeof<FSharpScriptLanguage>)>]
type SandboxDocumentLanguageSupportFSharpScript() =
    interface ISandboxDocumentLanguageSupport with
        member x.DocumentFileExtension = FSharpScriptProjectFileType.FsxExtension
        member x.ProjectFileType = FSharpScriptProjectFileType.Instance :> ProjectFileType
        member x.SetupSandboxFile(_, _, _) = ()
        member x.GetExtraInfos _ = null
