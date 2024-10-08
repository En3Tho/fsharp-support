namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.LanguageService

open System
open System.Collections.Generic
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open JetBrains.ProjectModel
open JetBrains.ReSharper.Intentions.QuickFixes
open JetBrains.ReSharper.Plugins.FSharp.Checker
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl
open JetBrains.ReSharper.Plugins.FSharp.Psi.Metadata
open JetBrains.ReSharper.Plugins.FSharp.Psi.Resolve
open JetBrains.ReSharper.Plugins.FSharp.Psi.Searching
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Util
open JetBrains.ReSharper.Plugins.FSharp.Util
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ExtensionsAPI.Finder
open JetBrains.ReSharper.Psi.Modules
open JetBrains.ReSharper.Psi.Tree

[<Language(typeof<FSharpLanguage>)>]
type FSharpImportTypeHelper() =
    let [<Literal>] opName = "FSharpImportTypeHelper.FindTypeCandidates"

    let isApplicable (context: IFSharpReferenceOwner) =
        let referenceName = context.As<ITypeReferenceName>()
        isNull (OpenStatementNavigator.GetByReferenceName(referenceName))

    interface IImportTypeHelper with
        member x.FindTypeCandidates(reference, importTypeCacheFactory) =
            let reference = reference.As<FSharpSymbolReference>()
            if isNull reference then Seq.empty else

            let context = reference.GetElement()
            if not (isApplicable context) then Seq.empty else

            let sourceFile = context.GetSourceFile()
            let psiModule = context.GetPsiModule()
            let containingModules = getContainingModules context
            let referenceStartOffset = context.GetDocumentStartOffset()

            let fsFile = sourceFile.FSharpFile
            let settings = fsFile.GetSettingsStoreWithEditorConfig()

            let names =
                if reference.IsQualified then

                    // find previous type reference and check if it is used
                    // If previous type ref is a method call then this this blow?

                    let ownerRef =
                        match reference.QualifierReference with
                        | null ->
                            match context.FirstChild with
                            // Find a way to do this via navigator
                            | :? IPrefixAppExpr as prefixAppExpr ->
                                match prefixAppExpr.FirstChild with
                                | :? IFSharpReferenceOwner as ownerRef ->
                                    ownerRef.Reference
                                | _ ->
                                    null
                            | _ ->
                                null
                        | qualifier ->
                            qualifier

                    if ownerRef = null then
                        [||] :> IList<string>
                    else

                    let qual = ownerRef.GetFcsSymbol()
                    let qualType =
                        match qual with
                        | :? FSharpMemberOrFunctionOrValue as mfv ->
                            if mfv.IsFunction || mfv.IsMember then
                                let mutable returnType = mfv.FullType
                                while returnType.GenericArguments.Count > 1 do
                                    returnType <- returnType.GenericArguments[1]
                                returnType
                            else
                                mfv.FullType
                            |> Some
                        | :? FSharpEntity as entity ->
                            entity.AsType()
                            |> Some
                        | _ ->
                            None

                    match qualType with
                    | None ->
                        [||] :> IList<string>
                    | Some typeOfQualifiedRef ->
                        let checkResults = fsFile.GetParseAndCheckResults(false, "abc").Value

                        let assemblySignature = checkResults.CheckResults.PartialAssemblySignature
                        let referenceAssemblies = checkResults.CheckResults.ProjectContext.GetReferencedAssemblies()

                        let allSignatures = [
                            assemblySignature
                            for referenceAssembly in referenceAssemblies do
                                referenceAssembly.Contents
                        ]

                        let asmList = [
                            for assemblySignature in allSignatures do
                                yield! AssemblyContent.GetAssemblySignatureContent AssemblyContentType.Full assemblySignature
                        ]

                        let refName = reference.GetName()
                        let typeOfQualifiedRef = typeOfQualifiedRef |> FSharpSymbolUtil.getAbbreviatedType
                        let typeRefName = typeOfQualifiedRef.BasicQualifiedName.Split('.') |> Array.last

                        let isExtensionMemberForQualifiedType (mfv: FSharpMemberOrFunctionOrValue) =
                            if mfv.IsInstanceMember || mfv.Attributes |> containsExtensionAttribute then
                                mfv.FullType.GenericArguments[0].Equals(typeOfQualifiedRef)
                            else
                                // isStatic F# extension
                                mfv.CompiledName.StartsWith(typeRefName, StringComparison.Ordinal)

                        let isApplicableModuleOrClass (entity: FSharpEntity) =
                            entity.TryGetMembersFunctionsAndValues()
                            |> Seq.exists (fun mfv ->
                                isExtensionMemberMfv mfv
                                && mfv.DisplayName.Equals(refName, StringComparison.Ordinal)
                                && isExtensionMemberForQualifiedType mfv)

                            // how to find type which these members belong to?
                            // F# compiled methods go like Console.MethName.Static // or instance?

                        let entityIsModuleOrClassThatContainsExtensionAttribute (entity: FSharpEntity) =
                            if entity.IsAttributeType || entity.IsNamespace then false else

                            entity.IsFSharpModule
                            || entity.IsClass && entity.Attributes |> containsExtensionAttribute

                        let entities = ResizeArray(256)

                        for asm in asmList do
                            match asm with
                            | { Symbol = :? FSharpEntity as entity } ->
                                if not entity.IsUnresolved
                                   && entityIsModuleOrClassThatContainsExtensionAttribute entity
                                   && isApplicableModuleOrClass entity then
                                       entities.Add(entity.DisplayName)
                            | _ ->
                                ()

                        entities :> IList<string>
                else
                    reference.GetAllNames().ResultingList()

            if names.Count = 0 then Seq.empty else

            let factory = importTypeCacheFactory.Invoke(context)

            let canReferenceInsideProject typeElement =
                let searchFilter = psiModule.GetSolution().GetComponent<FSharpSearchFilter>() :> ISearchFilter
                let elementKey = searchFilter.TryGetKey(typeElement)
                searchFilter.CanContainReferences(sourceFile, elementKey)

            let fsAssemblyAutoOpenCache = psiModule.GetSolution().GetComponent<FSharpAutoOpenCache>()

            let mutable candidates: ITypeElement seq =
                names
                |> Seq.collect factory.Invoke
                |> Seq.filter (fun clrDeclaredElement ->
                    let typeElement = clrDeclaredElement.As<ITypeElement>()
                    if isNull typeElement then false else

                    // todo: enable when singleton property cases are supported
                    if typeElement.IsUnionCase() then false else

                    if typeElement.Module == psiModule &&
                            not (canReferenceInsideProject typeElement) then false else

                    let moduleToOpen =
                        if reference.IsQualified then
                            clrDeclaredElement
                        else
                            getModuleToOpen typeElement

                    if containingModules.Contains(moduleToOpen) then false else

                    if typeElement.Module != psiModule &&
                            not (psiModule.References(typeElement.Module)) then
                        true else

                    let moduleToImport = ModuleToImport.DeclaredElement(moduleToOpen)
                    let moduleDecl, _ = findModuleToInsertTo fsFile referenceStartOffset settings moduleToImport
                    let qualifiedElementList = moduleToImport.GetQualifiedElementList(moduleDecl, true)
                    let names = qualifiedElementList |> List.map (fun el -> el.GetSourceName())

                    let autoOpenedModules = fsAssemblyAutoOpenCache.GetAutoOpenedModules(typeElement.Module)
                    if autoOpenedModules.Count > 0 && autoOpenedModules.Contains(String.concat "." names) then false else

                    let fsModule = typeElement.As<IFSharpModule>()
                    if isNotNull fsModule && isNotNull fsModule.AssociatedTypeElement then false else

                    let symbolUse = fsFile.CheckerService.ResolveNameAtLocation(context, names, false, opName)
                    Option.isSome symbolUse)
                |> Seq.cast


            let referenceName = context.As<ITypeArgumentOwner>()
            if isNotNull referenceName then
                let typeArgumentList = referenceName.TypeArgumentList
                if isNull typeArgumentList || typeArgumentList.TypeUsages.Count = 0 then () else

                let typesCount = typeArgumentList.TypeUsages.Count
                candidates <- candidates |> Seq.filter (fun c -> c.TypeParameters.Count = typesCount)

            let typeReferenceName = context.As<ITypeReferenceName>()
            if isNotNull (AttributeNavigator.GetByReferenceName(typeReferenceName)) then
                let attributeTypeElement = context.GetPredefinedType().Attribute.GetTypeElement()
                candidates <- candidates |> Seq.filter (fun c -> c.IsDescendantOf(attributeTypeElement))

            if isNotNull (InheritMemberNavigator.GetByTypeName(typeReferenceName)) then
                candidates <- candidates |> Seq.filter (fun c -> c :? IInterface || c :? IClass || c :? IStruct)

            candidates

        member x.ReferenceTargetCanBeType _ = true
        member x.ReferenceTargetIsUnlikelyBeType _ = false





// todo: ExtensionMethodImportUtilBase
