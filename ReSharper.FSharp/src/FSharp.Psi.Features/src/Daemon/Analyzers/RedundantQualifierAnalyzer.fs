module JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Analyzers.RedundantQualifierExpressionAnalyzer

open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl
open JetBrains.ReSharper.Plugins.FSharp.Util
open JetBrains.ReSharper.Plugins.FSharp.Util.FSharpAssemblyUtil
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ExtensionsAPI
open JetBrains.Util

let getOpens (context: IFSharpTreeNode): OneToSetMap<string, string> =
    let map = OneToSetMap()

    let psiModule = context.GetPsiModule()
    let symbolScope = getSymbolScope psiModule

    let getQualifiedName (element: IClrDeclaredElement) =
        toQualifiedList element |> List.map (fun el -> el.GetSourceName()) |> String.concat "."

    let import (element: IClrDeclaredElement) =
        map.Add(element.GetSourceName(), getQualifiedName element) |> ignore
        for autoImportedModule in getNestedAutoImportedModules element symbolScope do
            map.Add(autoImportedModule.GetSourceName(), getQualifiedName autoImportedModule) |> ignore

    for node in context.PathToRoot() do
        let moduleLikeDecl = node.As<IModuleLikeDeclaration>()
        if isNull moduleLikeDecl then () else

        for moduleMember in moduleLikeDecl.Members do
            let openStatement = moduleMember.As<IOpenStatement>()
            if isNull openStatement then () else

            let referenceName = openStatement.ReferenceName
            if isNull referenceName then () else

            let shortName = referenceName.ShortName
            if shortName = SharedImplUtil.MISSING_DECLARATION_NAME then () else

            let element = referenceName.Reference.Resolve().DeclaredElement.As<IClrDeclaredElement>()
            if isNull element then () else

            map.Add(shortName, referenceName.QualifiedName) |> ignore

            import element
    
        let topLevelDecl = moduleLikeDecl.As<ITopLevelModuleLikeDeclaration>()
        if isNotNull topLevelDecl then
            match topLevelDecl.DeclaredElement with
            | :? INamespace as ns -> import ns
            | :? ITypeElement as ty -> import (ty.GetContainingNamespace())
            | _ -> ()

    import symbolScope.GlobalNamespace

    map

[<ElementProblemAnalyzer(typeof<IReferenceExpr>, HighlightingTypes = [| typeof<RedundantQualifierWarning> |])>]
type RedundantQualifierExpressionAnalyzer() =
    inherit ElementProblemAnalyzer<IReferenceExpr>()

    let [<Literal>] OpName = "RedundantQualifierExpressionAnalyzer"

    override x.Run(refExpr, _, consumer) =
        let shortName = refExpr.ShortName
        if shortName = SharedImplUtil.MISSING_DECLARATION_NAME then () else

        let qualifierOwner = ReferenceExprNavigator.GetByQualifier(refExpr)
        if isNull qualifierOwner then () else

        let opens = getOpens refExpr
        if not (opens.GetValuesSafe(shortName) |> Seq.exists (endsWith refExpr.QualifiedName)) then () else

        let declaredElement =
            match qualifierOwner.Reference.Resolve().DeclaredElement with
            | :? IConstructor as ctor -> ctor.GetContainingType() :> IDeclaredElement
            | declaredElement -> declaredElement

        let checkerService = qualifierOwner.CheckerService
        match checkerService.ResolveNameAtLocation(qualifierOwner, [qualifierOwner.ShortName], OpName) with
        | None -> ()
        | Some symbolUse ->

        let unqualifiedElement = symbolUse.Symbol.GetDeclaredElement(qualifierOwner.GetPsiModule(), qualifierOwner)
        if declaredElement = unqualifiedElement then
            consumer.AddHighlighting(RedundantQualifierWarning(qualifierOwner))
