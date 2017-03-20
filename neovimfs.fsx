// ===========================================================================
//  FILE    : neovimfs.fsx
//  AUTHOR  : callmekohei <callmekohei at gmail.com>
//  License : MIT license
// ===========================================================================

#load @"./deopletefs.fsx"
#load @"./quickrunfs.fsx"

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
open FSharp.Control.Reactive.Observable

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



module private Suave =
    
    open quickrunfs 
    open quickrunfs.FsharpInteractive
    open deopletefs
    open deopletefs.FSharpIntellisence
    open deopletefs.Util

    let private evalScript (fsi:Fsi) =

        GET >=> pathScan "/evalScript/%s" ( fun fp ->
            try
                // switch stdout to memory stream
                use ms = new MemoryStream()
                use sw = new StreamWriter(ms)
                use tw = TextWriter.Synchronized(sw)

                sw.AutoFlush <- true
                Console.SetOut(tw)

                fsi.EvalScript(fp) |> ignore

                use sr = new System.IO.StreamReader(ms)
                ms.Position <- int64 0

                OK ( sr.ReadToEnd() ) 
            with
                | :? System.ArgumentException as ex ->
                    OK ( "(FSI: )" + ex.Message )
        )


    let private autoComplete (fsc:FSharpChecker) ( dic :ConcurrentDictionary<string,string> ) ( gen : Generator ) =

        let extractJson ctx key =
            match ctx.request.formData key with
            | Choice1Of2 x -> x
            | _            -> ""
        
        POST >=> path "/autoComplete" >=> ( fun (ctx: HttpContext) ->
            try
                let postData:PostData = {
                    Row      = extractJson ctx "row"
                    Col      = extractJson ctx "col"
                    Line     = extractJson ctx "line"
                    FilePath = extractJson ctx "filePath"
                    Source   = extractJson ctx "source"
                }

                // stdout.WriteLine( "neovimfs: " + postData.Line )
                
                if      Seq.isEmpty dic.Keys
                then    asyncInit fsc dic postData   |> Async.RunSynchronously
                elif    dic.Item( "filePath" ) <> postData.FilePath
                then    asyncReInit fsc dic postData |> Async.Start

                if
                        postData.Line.Split(' ')
                        |> Array.filter ( fun s -> s <> "" )
                        |> Array.last
                        |> fun s -> s.Contains(".")
                
                then    OK (dotHints fsc dic postData ) ctx
                else    gen.ReCashOneWordHints postData
                        OK ( oneWordOrAttributeHints dic postData ) ctx
                    
            with e ->
                let s = jsonSerializer.PickleToString( { header = "aaa" ; data = "(err): " + e.Message } )
                OK s ctx 
        )

    let errorAction = 
        fun _ -> async { return failwith "Uncaught exception!!" }

    let customErrorHandler ex msg ctx =
        let jsonSerializer:JsonSerializer = FsPickler.CreateJsonSerializer(indent = false, omitHeader = true) 
        let s = jsonSerializer.PickleToString( { word = msg; info=[[""]] } )
        OK s ctx

    let config = {
        defaultConfig with
            maxOps = 1
            errorHandler = customErrorHandler
    }

    let private app (fsiPath:string) =

        let fsi  = Fsi(fsiPath)
        let fsc  = FSharpChecker.Create()
        let sb   = new StringBuilder("")
        let dic : ConcurrentDictionary<string,string> = new ConcurrentDictionary< string, string >()
        let gen  = new Generator(1500.,  asyncReOneWordHints fsc dic)

        choose [ evalScript    fsi
                 autoComplete  fsc dic gen
                 GET >=> path "/error" >=> errorAction
                 NOT_FOUND     "Resource not found." ]

    [<EntryPointAttribute>]
    let main argv =
        let fsiPath = argv.[0]
        startWebServer config ( app fsiPath )
        0
