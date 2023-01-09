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
    "FSharp.Fantomas.Host",
    "FSharp.TypeProviders.Host"    

$dlls =
    "FSharp.Compiler.Service",
    "FSharp.Compiler.Interactive.Settings",
    "FSharp.DependencyManager.Nuget",
    "FSharp.Core"

$tfms =
    "net472",
    "net6.0"

function CopyReSharperDlls {
    $projects | foreach {
        $current = $_
        $tfms | foreach {
            $tfm = $_
            $path = "ReSharper.FSharp\src\$current\bin\Debug\$tfm\"
            if ((Test-Path -Path $path) -eq $True) {
                Copy-Item "ReSharper.FSharp\src\$current\bin\Release\$tfm\JetBrains.ReSharper.Plugins.$current.dll" -Destination $path
            }
        }
    }    
    $hosts | foreach {
        $current = $_
        $tfms | foreach {
            $tfm = $_
            $path = "ReSharper.FSharp\src\$current\bin\Debug\$tfm\"
            if ((Test-Path -Path $path) -eq $True) {
                Copy-Item "ReSharper.FSharp\src\$current\bin\Release\$tfm\JetBrains.ReSharper.Plugins.$current.exe" -Destination $path
            }
        }
        
    }
}

function CopyFSharpDlls {
    $dlls | foreach {
        $dll = $_
        ($projects + $hosts) | foreach {
            $current = $_
            $tfms | foreach {
                $tfm = $_
                $path = "ReSharper.FSharp\src\$current\bin\Debug\$tfm\"
                if ((Test-Path -Path $path)-eq $True) {
                    Copy-Item "..\visualfsharp\artifacts\bin\$dll\Release\netstandard2.0\$dll.dll" -Destination $path
                }
            }
        }
    }
}

CopyReSharperDlls
CopyFSharpDlls

Write-Host "0"