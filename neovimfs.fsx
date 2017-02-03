open System
open System.IO
open System.Text
open System.Text.RegularExpressions

#r @"./packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
open Microsoft.FSharp.Compiler
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


    // open FSharp.Data ---> ["FSharp"; "Data"]
    let openString (source: string)  =

        source.Split('\n')
        |> Array.filter ( fun (s:string) -> s.Contains("open") && not (s.Contains("//")) )
        |> Array.map ( fun s ->
            System.Text.RegularExpressions.Regex.Replace( s, "^.*open\s","")
            |> fun str ->
                if    str.Contains(".")
                then  str.Split(' ')
                        |> Array.last
                        |> fun (s:string) ->
                            if    s.Contains(".")
                            then  s.Split('.') |> Array.toList
                            else  [s]
                else  [str])
        |> Array.toList


    let qualifiedAndPartialNames (fp:string) (line:string)  =

            // "System.String."  ---> ["System"; "String"]
            let wordList =
                if    line.Contains(".")
                then  line.Split(' ')
                      |> Array.last
                      |> fun (s:string) -> s.Remove(s.Length - 1)
                      |> fun (s:string) ->
                          if    s.Contains(".")
                          then  s.Split('.') |> Array.toList
                          else  [s]
                else  [line]

            let partialName = wordList.[List.length wordList - 1]

            if      List.length wordList > 1
            then    [ ( wordList , "") ]
            else    let defaultSets    = [ ( [], wordList.[0]) ; ( wordList , "" ) ]
                    let defaultLibrary = [ ["Microsoft";"FSharp";"Collections"] ; ["Microsoft";"FSharp";"Core"] ]
                    let requireLibrary = openString(fp)
                    let l = match List.isEmpty requireLibrary with
                            | true  -> defaultLibrary                  |> List.map ( fun l -> l @ [partialName] )
                            | false -> defaultLibrary @ requireLibrary |> List.map ( fun l -> l @ [partialName] )

                    let emptyStringsList   = [ for i in 1 .. (List.length l) -> "" ]
                    let qualifingNamesList = (l,emptyStringsList) ||> List.zip

                    qualifingNamesList @ defaultSets


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


module FSharpAutoComplete =

    type FsChecker(checker:FSharpChecker, file:string, input:string, inputLines: string array) =

        let projOptions =
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        let parseFileResults, checkFileAnswer =
            checker.ParseAndCheckFileInProject(file, 0, input, projOptions)
            |> Async.RunSynchronously

        let checkFileResults =
            match checkFileAnswer with
            | FSharpCheckFileAnswer.Succeeded(res) -> res
            | res -> failwithf "Parsing did not finish... (%A)" res

        member x.decls (row:int, col:int, lst) =
            checkFileResults.GetDeclarationListInfo
                (Some parseFileResults, row, col, inputLines.[row - 1], (fst lst), (snd lst), fun _ -> false)
                |> Async.RunSynchronously


module Suave =
    open Util
    open FsharpInteractive
    open FSharpAutoComplete

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

    let autoComplete (fsc:FSharpChecker) =

        GET >=> pathScan "/autoComplete/%s" ( fun str ->

            // switch stdout to memory stream
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            use tw = TextWriter.Synchronized(sw)

            sw.AutoFlush <- true
            Console.SetOut(tw)

            let str2  = System.Web.HttpUtility.UrlDecode(str)
            let arr   = str2.Split( [|','|] , 5 )

            let row   = arr.[0]
            let col   = arr.[1]
            let line  = arr.[2]
            let fp    = arr.[3]
            let input = arr.[4]
            let inputLines = input.Split('\n')

            let lst = qualifiedAndPartialNames fp line
            let len = lst.Length
            let mutable i , flag = 1 , true

            while flag do
                let   info: FSharpDeclarationListInfo = FsChecker(fsc, fp, input,inputLines).decls(int(row), int(col), lst.[i-1] )

                if    info.Items.Length = 0
                then  i <- i + 1
                      if   len < i
                      then flag <- false
                      ()
                else  flag <- false
                      info.Items |> Array.iter ( fun x -> stdout.WriteLine(x.Name) )

            use sr = new System.IO.StreamReader(ms)
            ms.Position <- int64 0
            ()

            ; OK (sr.ReadToEnd()) )

    let app (fsiPath:string) =

        let fsi = Fsi(fsiPath)
        let fsc = FSharpChecker.Create()

        choose [ evalScript fsi
                 autoComplete fsc
                 NOT_FOUND "Resource not found." ]

    [<EntryPoint>]
    let main argv =
        let fsiPath = "/usr/local/Cellar/mono/4.6.2.7/lib/mono/fsharp/fsi.exe"
        startWebServer defaultConfig (app fsiPath)
        0

