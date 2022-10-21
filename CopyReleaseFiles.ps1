$projects = 
    "FSharp.Common",
    "FSharp.ProjectModelBase",
    "FSharp.Psi",
    "FSharp.Psi.Daemon",
    "FSharp.Psi.Features",
    "FSharp.Psi.Intentions",
    "FSharp.Psi.Services",
    "FSharp.TypeProviders.Protocol",    
    "FSharp.Fantomas.Protocol"    

$hosts =
    "FSharp.TypeProviders.Host",
    "FSharp.Fantomas.Host"

$dlls =
    "FSharp.Compiler.Service",
    "FSharp.Compiler.Interactive.Settings",
    "FSharp.DependencyManager.Nuget",
    "FSharp.Core"

function CopyReSharperDlls {
    $projects | foreach {
        Copy-Item "ReSharper.FSharp\src\$_\bin\Release\net472\JetBrains.ReSharper.Plugins.$_.dll" -Destination "ReSharper.FSharp\src\$_\bin\Debug\net472\"
    }
    $hosts | foreach {
        Copy-Item "ReSharper.FSharp\src\$_\bin\Release\net472\JetBrains.ReSharper.Plugins.$_.exe" -Destination "ReSharper.FSharp\src\$_\bin\Debug\net472\"
    }
}

function CopyFSharpDlls {
    $dlls | foreach {
        $dll = $_
        ($projects + $hosts) | foreach {
            Copy-Item "..\visualfsharp\artifacts\bin\$dll\Release\netstandard2.0\$dll.dll" -Destination "ReSharper.FSharp\src\$_\bin\Debug\net472\"
        }
    }
}

CopyReSharperDlls
CopyFSharpDlls

Write-Host "0"