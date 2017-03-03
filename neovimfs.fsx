// ===========================================================================
//  FILE    : neovimfs.fsx
//  AUTHOR  : callmekohei <callmekohei at gmail.com>
//  License : MIT license
// ===========================================================================

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Collections.Concurrent

#r @"./packages/FSharp.Control.Reactive/lib/net45/FSharp.Control.Reactive.dll"
#r @"./packages/Rx-Core/lib/net45/System.Reactive.Core.dll"
#r @"./packages/Rx-Linq/lib/net45/System.Reactive.Linq.dll"
#r @"./packages/Rx-Interfaces/lib/net45/System.Reactive.Interfaces.dll"
#r @"./packages/Rx-PlatformServices/lib/net45/System.Reactive.PlatformServices.dll"
open FSharp.Control.Reactive

#r @"./packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell

#r @"./packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json

#r @"./packages/FsPickler/lib/net45/FsPickler.dll"
#r @"./packages/FsPickler.Json/lib/net45/FsPickler.Json.dll"
open MBrace.FsPickler
open MBrace.FsPickler.Json

#r @"./packages/Suave/lib/net40/Suave.dll"
open Suave
open Suave.Operators
open Suave.Successful
open Suave.Filters
open Suave.RequestErrors


module private FsharpInteractive =

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

                

(*----------------------------------------------------------------------------------

                              intellisense

-----------------------------------------------------------------------------------*)


type PostData   = { Row:string; Col:string; Line:string; FilePath:string; Source:string }

type JsonFormat = { word : string; info: string list list  }

type SetOperationEventArgs<'a>(postData:PostData) =
    inherit System.EventArgs()
    member this.PS = postData 

type dlg = delegate of obj * SetOperationEventArgs<PostData> -> unit

type Generator() =
    let    m_event = new Event<dlg, SetOperationEventArgs<PostData>>()
    member this.Evnt      = m_event.Publish
    member this.OnEvnt (x) = m_event.Trigger(this, new SetOperationEventArgs<_>(x))


module Util =

    let angleBracket ( s:string ) :string =
        s.Split([|'<';'>'|])
        |> Array.filter ( fun s -> s <> "" )
        |> fun arr ->
           if     Array.last arr = "."
           then   Array.head arr
           else   (Array.tail arr).[0]

    let nameSpaceArrayImpl ( s:string) :string array =
        s.Split('.')
        |> Array.filter ( fun s -> s <> "" )
        |> fun arr ->
            if    Array.last arr = ""
            then  Array.splitAt (arr.Length - 1) arr |> fst
            else  arr

    let nameSpaceArray (s:string) : string array =
        s
        |> fun s -> s.Split(' ')
        |> Array.filter ( fun s -> s <> "" )
        |> Array.last
        |> fun ( s:string) ->
            if    s.Contains("<")
            then  angleBracket s |> nameSpaceArrayImpl
            elif  s.Contains(".")
            then  nameSpaceArrayImpl s
            else  [|s|]
        |> Array.map( fun s -> Regex.Replace(s,"\(.*\)","") )

    let extractJson ctx key =
        match ctx.request.formData key with
        | Choice1Of2 x -> x
        | _            -> ""
    

    let extractGroupTexts = function
        | FSharpToolTipElement.None                    -> []
        | FSharpToolTipElement.Single (a,b)            -> [a]
        | FSharpToolTipElement.SingleParameter (a,b,c) -> []
        | FSharpToolTipElement.Group xs                -> xs |> List.map fst
        | FSharpToolTipElement.CompositionError s      -> [s]

    let asyncGetDeclarationListInfo ( checker:FSharpChecker) ( ps:PostData ) (arr:(string array * string)) : Async<FSharpDeclarationListInfo> = async {

        let! (projOptions : FSharpProjectOptions) =
            checker.GetProjectOptionsFromScript( ps.FilePath , ps.Source )

        // Perform parsing and type checking 
        let! (parseFileResults : FSharpParseFileResults) , (checkFileAnswer: FSharpCheckFileAnswer) =
            checker.ParseAndCheckFileInProject( ps.FilePath, 0, ps.Source, projOptions )

        let checkFileResults = 
            match checkFileAnswer with
            | FSharpCheckFileAnswer.Succeeded(res) -> res
            | res -> failwithf "Parsing did not finish... (%A)" res

        return! checkFileResults.GetDeclarationListInfo ( Some parseFileResults, int(ps.Row) , int(ps.Col) , ps.Line, (fst arr) |> Array.toList, (snd arr), fun _ -> false )
    }


module  FSharpIntellisence  =
    open Util

    let jsonSerializer:JsonSerializer = FsPickler.CreateJsonSerializer(indent = false, omitHeader = true) 

    let jsonStrings (fsc:FSharpChecker) (postData:PostData) (nameSpace: string [] )  (word:string) : string =
        asyncGetDeclarationListInfo fsc postData ( nameSpace, word ) |> Async.RunSynchronously |> fun x -> x.Items
        |> Array.fold ( fun state x ->
            let dt : JsonFormat = { word = x.Name; info = match x.DescriptionText with FSharpToolTipText xs -> List.map extractGroupTexts xs }
            state + "\n" + jsonSerializer.PickleToString( dt ) ) ""

    let asyncInit (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData) : Async<unit> = async {
        dic.GetOrAdd( "filePath" , postData.FilePath ) |> ignore 

        // Do not use Array.Parallel.iter because too late !
        [|"System" ; "List" ; "Set" ; "Seq" ; "Array" ; "Map" ; "Option" |]
        |> Array.iter ( fun (s:string) -> dic.GetOrAdd ( s, jsonStrings fsc postData [|s|] "" ) |> ignore )

        dic.GetOrAdd( "OneWordHint" , jsonStrings fsc postData [||] "" ) |> ignore 
    }

    let asyncReInit (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData) : Async<unit> = async {
        dic.TryUpdate( "filePath"   , postData.FilePath                        , dic.Item("filePath")    ) |> ignore
        dic.TryUpdate( "System"     , jsonStrings fsc postData [|"System"|] "" , dic.Item("System")      ) |> ignore 
        dic.TryUpdate( "OneWordHint", jsonStrings fsc postData [||] ""         , dic.Item("OneWordHint") ) |> ignore 
    }

    let asyncReOneWordHint (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData) : Async<unit> = async {
        
        // dic.TryUpdate( "OneWordHint", jsonStrings fsc postData [||] ""         , dic.Item("OneWordHint") ) |> ignore 

        let x = jsonStrings fsc postData [||] ""

        dic.AddOrUpdate( "OneWordHint", x , fun key existingVal ->
            if      x <> existingVal
            then    x
            else    dic.Item("OneWordHint")
        ) |> ignore

    }

    let public intellisense (fsc:FSharpChecker) (ctx: HttpContext) ( dic : ConcurrentDictionary<string,string> ) ( gen : Generator ) : string =
 
        let postData:PostData = {
            Row      = extractJson ctx "row"
            Col      = extractJson ctx "col"
            Line     = extractJson ctx "line"
            FilePath = extractJson ctx "filePath"
            Source   = extractJson ctx "source" }

        let dotHint () : string =
            let arr = nameSpaceArray postData.Line
            match Array.last arr with
            | "System" | "List" | "Set" | "Seq" | "Array" | "Map" | "Option" ->
                dic.Item( Array.last arr )
            | _ ->
                asyncGetDeclarationListInfo fsc postData ( arr ,"" ) |> Async.RunSynchronously |> fun x -> x.Items
                |> Array.fold ( fun state x ->
                    let dt : JsonFormat = { word = x.Name; info = match x.DescriptionText with FSharpToolTipText xs -> List.map extractGroupTexts xs }
                    state + "\n" + jsonSerializer.PickleToString( dt ) ) ""

        let oneWordHint (s:string) : string =
            try
                dic.Item( "OneWordHint" )
                |> fun str -> str.Split('\n')
                |> Array.filter ( fun str -> Regex.IsMatch( str.ToLower() , "(?<={\"word\":\")" + s.ToLower() + ".*" ))
                |> Array.reduce ( fun a b -> a + "\n" + b )
            with
                | :? System.ArgumentException -> "" 

        let attributeHint (s:string) : string =
            try
                dic.Item( "OneWordHint" )
                |> fun str -> str.Split('\n')
                |> Array.filter ( fun str -> str.Contains("Attribute") )
                |> Array.reduce ( fun a b -> a + "\n" + b )
            with
                | :? System.ArgumentException -> ""

        let abc (): string =
            postData.Line.Split(' ')
            |> Array.filter ( fun s -> s <> "" )
            |> fun ary ->
               if    Array.contains "[<" ary  && not ( Array.contains ">]" ary )
               then  attributeHint ( Array.last ary |> fun s -> s.Replace( "[<","" ) )
               else  oneWordHint   ( Array.last ary )


        if      Seq.isEmpty dic.Keys
        then    asyncInit fsc dic postData   |> Async.RunSynchronously
        elif    dic.Item( "filePath" ) <> postData.FilePath
        then    asyncReInit fsc dic postData |> Async.Start 

        if      postData.Line.Contains(".")
        then    dotHint    ()
        else    gen.OnEvnt postData
                abc ()

        



module private Suave =

    open FsharpInteractive
    open FSharpIntellisence


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

            OK ( sr.ReadToEnd() ) )

    let private autoComplete (fsc:FSharpChecker) ( dic :ConcurrentDictionary<string,string> ) ( gen : Generator ) =

        POST >=> path "/autoComplete" >=> ( fun (ctx: HttpContext) ->
            OK (intellisense fsc ctx dic gen ) ctx )



    let private app (fsiPath:string) =

        let fsi = Fsi(fsiPath)
        let fsc = FSharpChecker.Create()
        let dic : ConcurrentDictionary<string,string> = new ConcurrentDictionary< string, string >()
        let gen = new Generator()

        gen.Evnt
        |> Observable.throttle  ( System.TimeSpan.FromMilliseconds(2000.))
        |> Observable.subscribe ( fun e -> asyncReOneWordHint fsc dic e.PS |> Async.Start )
        |> ignore 

        choose [ evalScript    fsi
                 autoComplete  fsc dic gen
                 NOT_FOUND     "Resource not found." ]


    [<EntryPointAttribute>]
    let private main argv =
        let fsiPath = argv.[0]
        startWebServer defaultConfig (app fsiPath)
        0

