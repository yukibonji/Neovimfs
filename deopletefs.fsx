// ===========================================================================
//  FILE    : deopletefs.fsx
//  AUTHOR  : callmekohei <callmekohei at gmail.com>
//  License : MIT license
// ===========================================================================

namespace deopletefs

open System.Text.RegularExpressions
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

#r @"./packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json

#r @"./packages/FsPickler/lib/net45/FsPickler.dll"
#r @"./packages/FsPickler.Json/lib/net45/FsPickler.Json.dll"
open MBrace.FsPickler
open MBrace.FsPickler.Json


type PostData   = { Row:string; Col:string; Line:string; FilePath:string; Source:string }
type JsonOuter  = { header:string; data: string }
type JsonFormat = { word : string; info: string list list  }

type Generator( time:float, func: PostData -> Async<unit> ) =
    let m_Event = new Event<PostData>()

    do
        m_Event.Publish
        |> Observable.throttle  ( System.TimeSpan.FromMilliseconds(time) )
        |> Observable.subscribe ( func >> Async.Start )
        |> ignore 

    member this.ReCashOneWordHints (x) = m_Event.Trigger(x)

module Util =

    let jsonSerializer:JsonSerializer = FsPickler.CreateJsonSerializer(indent = false, omitHeader = true) 

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

    let jsonStrings (fsc:FSharpChecker) (postData:PostData) (nameSpace: string [] )  (word:string) : string =
        try
            let tmp =
                asyncGetDeclarationListInfo fsc postData ( nameSpace, word ) |> Async.RunSynchronously |> fun x -> x.Items
                |> Array.fold ( fun state x ->
                    let dt : JsonFormat = { word = x.Name; info = match x.DescriptionText with FSharpToolTipText xs -> List.map extractGroupTexts xs }
                    state + "\n" + jsonSerializer.PickleToString( dt ) ) ""

            jsonSerializer.PickleToString( {header = "jsonStrings"; data = tmp.TrimEnd() })
        
        with e ->
            jsonSerializer.PickleToString( { header = "jsonStrings err" ; data = "(js err): " + e.Message } )
    
    let asyncInit (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData) : Async<unit> = async {
        dic.GetOrAdd( "filePath" , postData.FilePath ) |> ignore 

        // Do not use Array.Parallel.iter because too late !
        [|"System" ; "List" ; "Set" ; "Seq" ; "Array" ; "Map" ; "Option" |]
        |> Array.iter ( fun (s:string) -> dic.GetOrAdd ( s, jsonStrings fsc postData [|s|] "" ) |> ignore )

        dic.GetOrAdd( "DotHints" , jsonSerializer.PickleToString( {header="DotHints";data=""}  ) ) |> ignore

        dic.GetOrAdd( "OneWordHint" , jsonStrings fsc postData [||] "" ) |> ignore 
    }

    let asyncReInit (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData) : Async<unit> = async {
        dic.TryUpdate( "filePath"   , postData.FilePath                        , dic.Item("filePath")    ) |> ignore
        dic.TryUpdate( "System"     , jsonStrings fsc postData [|"System"|] "" , dic.Item("System")      ) |> ignore 
        dic.TryUpdate( "OneWordHint", jsonStrings fsc postData [||] ""         , dic.Item("OneWordHint") ) |> ignore 
    }

    let asyncReOneWordHints (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData) : Async<unit> = async {
        try 
            let x = jsonStrings fsc postData [||] ""

            dic.AddOrUpdate( "OneWordHint", x , fun key existingVal ->
                if      x <> existingVal
                then    x
                else    dic.Item("OneWordHint")
            ) |> ignore

        with e ->
            jsonSerializer.PickleToString( { header = "asyncReOneWordHints err" ; data = "(aReOneword err): " + e.Message } ) |> ignore
    }

    let dotHints (fsc:FSharpChecker) (dic:ConcurrentDictionary<string,string>) (postData:PostData)  : string =
        let arr = nameSpaceArray postData.Line
        match Array.last arr with
        | "System" | "List" | "Set" | "Seq" | "Array" | "Map" | "Option" ->
            try 
                dic.Item( Array.last arr )
            with e ->
                jsonSerializer.PickleToString( { header = "aaa" ; data = "(dotHints_cash err): " + e.Message } )
        | _ ->
            try
                jsonStrings fsc postData arr ""
            with e ->
                jsonSerializer.PickleToString( { header = "aaa" ; data = "(dotHints err): " + e.Message } )

    


    let oneWordHints (dic:ConcurrentDictionary<string,string>)  (s:string) : string =
        
        let s1 = s.Replace("typeof<","")

        try
            let js = JsonConvert.DeserializeObject<JsonOuter>(dic.Item( "OneWordHint" ))
            
            let tmp =
                js.data
                |> fun str -> str.Split('\n')
                |> Array.filter ( fun str -> Regex.IsMatch( str.ToLower() , "(?<={\"word\":\")" + s1.ToLower() + ".*" ))
                |> Array.reduce ( fun a b -> a + "\n" + b )

            jsonSerializer.PickleToString( {header = "oneWordHints"; data = tmp.TrimEnd() })

        with e ->
            jsonSerializer.PickleToString( { header = "aaa" ; data = "(oneWord err): " + e.Message } )


    let attributeHints (dic:ConcurrentDictionary<string,string>)  (s:string) : string =
        try
            let js = JsonConvert.DeserializeObject<JsonOuter>(dic.Item( "OneWordHint" ))
            
            let tmp =
                js.data
                |> fun str -> str.Split('\n')
                |> Array.filter ( fun str -> str.Contains("Attribute") )
                |> Array.reduce ( fun a b -> a + "\n" + b )

            jsonSerializer.PickleToString( {header = "attributeHints"; data = tmp.TrimEnd() })

        with e ->
            jsonSerializer.PickleToString( { header = "aaa" ; data = "(attr err): " + e.Message } )

    let oneWordOrAttributeHints (dic:ConcurrentDictionary<string,string>) (postData:PostData) : string =
        postData.Line.Split(' ')
        |> Array.filter ( fun s -> s <> "" )
        |> fun ary ->
           if    Array.contains "[<" ary  && not ( Array.contains ">]" ary )
           then  attributeHints dic  ( Array.last ary |> fun s -> s.Replace( "[<","" ) )
           else  oneWordHints   dic  ( Array.last ary  )



    

