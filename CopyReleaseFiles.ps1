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

$pathPrefix = "ReSharper.FSharp\src\FSharp"
$hostPathPrefix = "ReSharper.FSharp\src"

function CopyReSharperDlls {
    $projects | foreach {
        $current = $_
        $tfms | foreach {
            $tfm = $_
            $path = "$pathPrefix\$current\bin\Debug\$tfm\"
            if ((Test-Path -Path $path) -eq $True) {
                Write-Host "Path found: $path"
                Copy-Item "$pathPrefix\$current\bin\Release\$tfm\JetBrains.ReSharper.Plugins.$current.dll" -Destination $path
            } else {
                Write-Host "Path not found: $path"
            }
        }
    }
    $hosts | foreach {
        $current = $_
        $tfms | foreach {
            $tfm = $_
            $path = "$hostPathPrefix\$current\bin\Debug\$tfm\"
            if ((Test-Path -Path $path) -eq $True) {
                Write-Host "Path found: $path"
                Copy-Item "$hostPathPrefix\$current\bin\Release\$tfm\JetBrains.ReSharper.Plugins.$current.exe" -Destination $path
            } else {
                Write-Host "Path not found: $path"
            }
        }
        
    }
}

function CopyFSharpDlls {
    $dlls | foreach {
        $dll = $_
        $projects | foreach {
            $current = $_
            $tfms | foreach {
                $tfm = $_
                $path = "$pathPrefix\$current\bin\Debug\$tfm\"
                if ((Test-Path -Path $path)-eq $True) {
                    Write-Host "Path found: $path"
                    Copy-Item "..\..\dotnet\fsharp\artifacts\bin\$dll\Release\netstandard2.0\$dll.dll" -Destination $path
                } else {
                    Write-Host "Path not found: $path"
                }
            }
        }

        $hosts | foreach {
            $current = $_
            $tfms | foreach {
                $tfm = $_
                $path = "$hostPathPrefix\$current\bin\Debug\$tfm\"
                if ((Test-Path -Path $path)-eq $True) {
                    Write-Host "Path found: $path"
                    Copy-Item "..\..\dotnet\fsharp\artifacts\bin\$dll\Release\netstandard2.0\$dll.dll" -Destination $path
                } else {
                    Write-Host "Path not found: $path"
                }
            }
        }
    }
}

CopyReSharperDlls
CopyFSharpDlls

Write-Host "0"