// ===========================================================================
//  FILE    : neovimfs.fsx
//  AUTHOR  : callmekohei <callmekohei at gmail.com>
//  License : MIT license
// ===========================================================================

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


module private FsharpInteractive =

    // Wrap text at arbitrary place
    let private orikaeshi (str:string) (returnPoint:int) =

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


        member public this.EvalScript (fp:string) =

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


module private FSharpAutoComplete =

    type public FsChecker( checker:FSharpChecker, file:string, input:string ) =

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

        member public x.decls ( row:int, col:int, inputLines: string array, arr:(string array * string) ) =
            checkFileResults.GetDeclarationListInfo
                (Some parseFileResults, row, col, inputLines.[row - 1], (fst arr) |> Array.toList, (snd arr), fun _ -> false)
                |> Async.RunSynchronously


    // typeof<System.       ---> System
    // typeof<System.Text   ---> System.Text
    // typeof<System.Math>. ---> typeof
    let private angleBracket (s:string) : string =
        if    Regex.Match(   s, "typeof<.*>\.").Success
        then  Regex.Replace( s, "<.*>\.", ""  )
        elif  Regex.Match(   s, "(?<=.*\<).*" ).Success
        then  Regex.Replace( s, "typeof<", "" ) |> fun s -> Regex.Replace( s , "\.$", "")
        else  ""


    // open FSharp.Data
    // open System.Text
    // //open System
    // [|[|"FSharp"; "Data"|]; [|"System"; "Text"|]|]
    let private openString (s:string)  =
        Regex.Split(s,"\n")
        |> Array.choose ( fun s ->
            match s with
            | s when Regex.Match(s,"open").Success && not (Regex.Match(s,"//").Success) ->
                Some ( Regex.Replace( s, "^.*open\s","")
                       |> fun s ->
                            if    Regex.Match(s,"\.").Success
                            then  Regex.Split(s,"\.")
                            else  [|s|] ) 
            | _ -> None )

    
    let private nameSpaceArrayImpl (s:string) : string array =
        if    Regex.Match(   s, "\.$").Success
        then  Regex.Replace( s, "\.$", "") |> fun s ->  Regex.Split(   s, "\." )
        else  Regex.Split(   s, "\." )


    // System.Text            ---> [| "System"; "Text" |]
    // System.Text.           ---> [| "System"; "Text" |]
    // Regex(",").Split(s,3)  ---> [| "Regex" ; "Split"|]   
    let private nameSpaceArray (s:string) : string array =
        s
        |> fun s -> Regex.Split( s , " " )
        |> Array.last
        |> fun (s:string) ->
            if    Regex.Match( s, "<" ).Success
            then  angleBracket s |> fun s -> nameSpaceArrayImpl s
            elif  Regex.Match( s, "\." ).Success
            then  nameSpaceArrayImpl s
            else  [|s|]
        |> Array.map( fun s -> Regex.Replace(s,"\(.*\)","") )


    // [| ( [|"Microsoft";"FSharp";"Collections";"List"|] ; "" ) ; ( [||] , "List" ) |]
    let public qualifiedNamesAndPartialName (fp:string) (line:string) : (string array * string) array  =
        
        let wordArr     : string [] = nameSpaceArray line
        let partialName : string    = Array.last wordArr

        if      Array.length wordArr > 1
        then    [| ( wordArr , "") |]
        else
                let defaultSets    = [| ( [||], wordArr.[0]) ; ( wordArr , "" ) |]
                let defaultLibrary = [| [|"Microsoft";"FSharp";"Collections"|] ; [|"Microsoft";"FSharp";"Core"|] |]
                let requireLibrary = openString(fp)

                let arr = match Array.isEmpty requireLibrary with
                            | true  -> defaultLibrary
                                       |> Array.map ( fun arr -> Array.append arr  [| partialName |] )
                            | false -> Array.append defaultLibrary requireLibrary
                                       |> Array.map ( fun arr -> Array.append arr  [| partialName |] )

                let emptyStringsArr   = [| for i in 1 .. (Array.length arr) -> "" |]
                let qualifingNamesArr = (arr,emptyStringsArr) ||> Array.zip

                Array.append qualifingNamesArr defaultSets


module private Suave =
    
    open FsharpInteractive
    open FSharpAutoComplete


    let private evalScript (fsi:Fsi) =

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


    let private autoComplete (fsc:FSharpChecker) =

        GET >=> pathScan "/autoComplete/%s" ( fun str ->

            // switch stdout to memory stream
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            use tw = TextWriter.Synchronized(sw)

            sw.AutoFlush <- true
            Console.SetOut(tw)

            let arr  = ( System.Web.HttpUtility.UrlDecode(str) ) |> fun s -> Regex(",@,").Split(s,5)

            let row         = arr.[0]
            let col         = arr.[1]
            let line        = arr.[2]
            let filePath    = arr.[3]
            let source      = arr.[4]
            let sourceLines = Regex.Split(source,"\n")

            let arr = qualifiedNamesAndPartialName filePath line
            let len = arr.Length
            let mutable i , flag = 1 , true

            while flag do

                let   info: FSharpDeclarationListInfo = FsChecker(fsc, filePath, source).decls(int(row), int(col), sourceLines, arr.[i-1] )

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


    let private app (fsiPath:string) =

        let fsi = Fsi(fsiPath)
        let fsc = FSharpChecker.Create()

        choose [ evalScript    fsi
                 autoComplete  fsc
                 NOT_FOUND     "Resource not found." ]


    [<EntryPointAttribute>]
    let private main argv =
        let fsiPath = "/usr/local/Cellar/mono/4.6.2.7/lib/mono/fsharp/fsi.exe"
        startWebServer defaultConfig (app fsiPath)
        0

