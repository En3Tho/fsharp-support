Copy-Item "ReSharper.FSharp\src\FSharp.Common\bin\Release\net461\JetBrains.ReSharper.Plugins.FSharp.Common.dll" -Destination "ReSharper.FSharp\src\FSharp.Common\bin\Debug\net461\"
Copy-Item "ReSharper.FSharp\src\FSharp.ProjectModelBase\bin\Release\net461\JetBrains.ReSharper.Plugins.FSharp.ProjectModelBase.dll" -Destination "ReSharper.FSharp\src\FSharp.ProjectModelBase\bin\Debug\net461\"
Copy-Item "ReSharper.FSharp\src\FSharp.Psi\bin\Release\net461\JetBrains.ReSharper.Plugins.FSharp.Psi.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi\bin\Debug\net461\"
Copy-Item "ReSharper.FSharp\src\FSharp.Psi.Features\bin\Release\net461\JetBrains.ReSharper.Plugins.FSharp.Psi.Features.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi.Features\bin\Debug\net461\"

Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.Compiler.Service.dll" -Destination "ReSharper.FSharp\src\FSharp.Common\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.Compiler.Service.dll" -Destination "ReSharper.FSharp\src\FSharp.ProjectModelBase\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.Compiler.Service.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.Compiler.Service.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi.Features\bin\Debug\net461\"

Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Interactive.Settings\Release\netstandard2.0\FSharp.Compiler.Interactive.Settings.dll" -Destination "ReSharper.FSharp\src\FSharp.Common\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Interactive.Settings\Release\netstandard2.0\FSharp.Compiler.Interactive.Settings.dll" -Destination "ReSharper.FSharp\src\FSharp.ProjectModelBase\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Interactive.Settings\Release\netstandard2.0\FSharp.Compiler.Interactive.Settings.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Interactive.Settings\Release\netstandard2.0\FSharp.Compiler.Interactive.Settings.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi.Features\bin\Debug\net461\"             

Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.DependencyManager.Nuget.dll" -Destination "ReSharper.FSharp\src\FSharp.Common\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.DependencyManager.Nuget.dll" -Destination "ReSharper.FSharp\src\FSharp.ProjectModelBase\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.DependencyManager.Nuget.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi\bin\Debug\net461\"
Copy-Item "..\visualfsharp\artifacts\bin\FSharp.Compiler.Service\Release\netstandard2.0\FSharp.DependencyManager.Nuget.dll" -Destination "ReSharper.FSharp\src\FSharp.Psi.Features\bin\Debug\net461\"

Write-Host "0"