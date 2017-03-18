// ===========================================================================
//  FILE    : quickrunfs.fsx
//  AUTHOR  : callmekohei <callmekohei at gmail.com>
//  License : MIT license
// ===========================================================================

namespace quickrunfs

open System.IO
open System.Text

#r @"./packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
open Microsoft.FSharp.Compiler.Interactive.Shell

module public FsharpInteractive =

    // Wrap text at arbitrary place
    let private orikaeshi (str:string) (returnPoint:int) : string =

        let sb = new System.Text.StringBuilder("")
        let mutable len = 0

        str
        |> Seq.iter( fun c ->
            len <- len + 1
            if    len >= returnPoint
            then  if    c = ' '
                  then  len <- 0
                        sb.AppendLine(string c) |> ignore
                  else  sb.Append(string c)     |> ignore
            else  sb.Append(string c) |> ignore )

        sb.ToString()

    type public Fsi (fsiPath:string) =

        // Intialize output and input streams
        let sbOut      = new StringBuilder()
        let sbErr      = new StringBuilder()
        let inStream   = new StringReader("")
        let outStream  = new StringWriter(sbOut)
        let errStream  = new StringWriter(sbErr)

        // Build command line arguments & start FSI session
        let argv       = [| fsiPath |]
        let allArgs    = Array.append argv [|"--noninteractive"|]
        let fsiConfig  = FsiEvaluationSession.GetDefaultConfiguration()
        let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)


        member public this.EvalScript (fp:string) : unit =
            try
                let fp = System.Web.HttpUtility.UrlDecode(fp)
                let result, warnings = fsiSession.EvalScriptNonThrowing fp

                sbOut.Clear() |> ignore
                sbErr.Clear() |> ignore

                match result with
                | Choice1Of2 ()  -> ()
                | Choice2Of2 exn ->

                    exn.Message + "\n" |> stdout.WriteLine

                    warnings
                    |> Array.map ( fun w ->

                        let severity = match w.Severity with
                                       | Microsoft.FSharp.Compiler.FSharpErrorSeverity.Error   -> "error"
                                       | Microsoft.FSharp.Compiler.FSharpErrorSeverity.Warning -> "warning"

                        "("          +  string w.StartLineAlternate + "-" + string w.StartColumn + ")"
                             +  " "  +  w.Subcategory
                             +  " "  +  severity
                             +  " "  +  "error:" + System.String.Format( "FS{0:0000}" , w.ErrorNumber )
                             +  "\n" +  (orikaeshi w.Message 65)
                             +  "\n"
                        )

                    |> Array.sort
                    |> Array.distinct
                    |> Array.iter ( fun s -> stdout.WriteLine(s) )
            
            with e ->
                stdout.WriteLine ( "FSI err: " + e.Message )
