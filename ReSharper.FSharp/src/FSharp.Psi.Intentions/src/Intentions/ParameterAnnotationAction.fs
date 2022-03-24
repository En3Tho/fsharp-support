namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Intentions

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Feature.Services.ContextActions
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Psi.ExtensionsAPI
open JetBrains.ReSharper.Resources.Shell

[<ContextAction(Name = "AnnotateBinding", Group = "F#",
                Description = "Annotate binding with explicit type")>]
type ParameterAnnotationAction(dataProvider: FSharpContextActionDataProvider) =
    inherit FSharpContextActionBase(dataProvider)

    let rec getBinding (pat: IFSharpTreeNode) =
        match pat.Parent with
        | :? IBinding as binding -> binding
        | :? IFSharpTreeNode as node -> getBinding node
        | _ -> null

    override this.IsAvailable _ =
        let localReference = dataProvider.GetSelectedElement<ILocalReferencePat>()
        if isNull localReference then false
        elif localReference.Parent :? ITypedPat || localReference.Parent :? IUnitPat then false else
        true

    override this.Text = "Add type annotationsX"

    override x.ExecutePsiTransaction _ =
        let refPat = dataProvider.GetSelectedElement<ILocalReferencePat>()

        use writeCookie = WriteLockCookie.Create(refPat.IsPhysical())
        use disableFormatter = new DisableCodeFormatter()

        let symbolUse = refPat.GetFcsSymbolUse()
        if isNull symbolUse then () else

        let mfv = symbolUse.Symbol :?> FSharpMemberOrFunctionOrValue
        let displayContext = symbolUse.DisplayContext

        match refPat.Binding with
        | :? ILocalBinding as localBinding ->
            () // if let or use token then specify local else specify parameter
        | _ ->
            ()

        SpecifyTypes.specifyParameterType displayContext mfv.FullType refPat