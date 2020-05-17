namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.QuickFixes

open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.QuickFixes
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Util
open JetBrains.ReSharper.Psi.ExtensionsAPI
open JetBrains.ReSharper.Psi.ExtensionsAPI.Tree
open JetBrains.ReSharper.Resources.Shell

type RemoveRedundantQualifierFix(warning: RedundantQualifierWarning) =
    inherit FSharpQuickFixBase()

    let treeNode = warning.TreeNode
    
    override x.Text = "Remove redundant qualifier"

    override x.IsAvailable _ =
        isValid treeNode

    override x.ExecutePsiTransaction _ =
        use writeCookie = WriteLockCookie.Create(treeNode.IsPhysical())
        use disableFormatter = new DisableCodeFormatter()

        match treeNode with
        | :? IReferenceExpr as refExpr ->
            ModificationUtil.DeleteChildRange(refExpr.Qualifier, refExpr.Identifier.PrevSibling)

        | _ -> failwith "todo"