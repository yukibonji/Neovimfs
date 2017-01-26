open System
open System.IO
open System.Text

#r @"./packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell

#r @"./packages/Suave/lib/net40/Suave.dll"
open Suave
open Suave.Operators
open Suave.Successful
open Suave.Filters
open Suave.RequestErrors

module Util =

    // Wrap text at arbitrary place
    let orikaeshi (str:string) (returnPoint:int) =

        let sb = new System.Text.StringBuilder("")
        let mutable len = 0

        str
        |> Seq.iter( fun c ->
            len <- len + 1
            if len >= returnPoint
            then
                if   c = ' '
                then len <- 0
                     sb.AppendLine(string c) |> ignore
                else sb.Append(string c)     |> ignore
            else     sb.Append(string c)     |> ignore )

        sb.ToString()


module FsharpInteractive =
    open Util

    type Fsi (fsiPath:string) =

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


        member this.EvalScript (fp:string) =

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

                    let sb = new System.Text.StringBuilder()

                    let severity = match w.Severity with
                                   | Microsoft.FSharp.Compiler.FSharpErrorSeverity.Error   -> "error"
                                   | Microsoft.FSharp.Compiler.FSharpErrorSeverity.Warning -> "warning"

                    sb.Append(
                           "("  +  string w.StartLineAlternate + "-" + string w.StartColumn + ")"
                        +  " "  +  w.Subcategory
                        +  " "  +  severity
                        +  " "  +  "error:" + System.String.Format( "FS{0:0000}" , w.ErrorNumber )
                        +  "\n" +  (orikaeshi w.Message 65)
                        +  "\n"
                    ) |> ignore

                    sb.ToString() )

                |> Array.sort
                |> Array.distinct
                |> Array.iter ( fun s -> stdout.WriteLine(s) )


module Suave =
    open FsharpInteractive

    let evalScript (fsi:Fsi) =

        GET >=> pathScan "/evalScript/%s" ( fun fp ->

            // switch stdout to memory stream
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            use tw = TextWriter.Synchronized(sw)

            sw.AutoFlush <- true
            Console.SetOut(tw)

            fsi.EvalScript(fp) |> ignore

            use sr = new System.IO.StreamReader(ms)
            ms.Position <- int64 0
            ()

            ; OK (sr.ReadToEnd()) )

    let app (fsiPath:string) =

        let fsi = Fsi(fsiPath)

        choose [ evalScript fsi
                 NOT_FOUND "Resource not found." ]

    [<EntryPoint>]
    let main argv =
        let fsiPath = "/usr/local/Cellar/mono/4.6.2.7/lib/mono/fsharp/fsi.exe"
        startWebServer defaultConfig (app fsiPath)
        0

