namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Daemon.Analyzers

// method call
// (Log*Information/Debug/Warning etc) calls where (const format) ->
// parse format holes and (args) -> warn if 1) format holes count and args count do no match, format holes names and arg names do not match?


// TODO: try warn if logging format holes are not fully used