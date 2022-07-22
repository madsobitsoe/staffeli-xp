open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open FsHttp
open FsHttp.DslCE
open FsHttp.Response

open Parsers.InputParser


// Not used
let accessTokenEndPoint = "https://absalon.ku.dk/api/v1/inst_access_tokens"
// Endpoint for canvas graphql
let endpoint = "https://absalon.ku.dk/api/graphql"


FsHttp.GlobalConfig.Json.defaultJsonSerializerOptions <-
     let options = JsonSerializerOptions()
     options.Converters.Add(JsonFSharpConverter())
     options

type AccessToken = { token : string }




let readToken file =
    File.ReadAllText file

// Not used, but example of how to deserialize Json into F# types
let getInstAccessToken token =
    http {
        POST accessTokenEndPoint
        header "Accept" "application/json"
        header "Authorization" (sprintf "Bearer %s" token)
    }
    |> Request.send
    |> Response.deserializeJson<AccessToken>
    

let sendQuery token =
    let query = http {
        POST endpoint
        header "Accept" "application/json"
        header "Authorization" (sprintf "Bearer %s" token)
        body
        formUrlEncoded [
            "query", "{ allCourses { id courseCode _id } }"
        ]
    }
    query |> printfn "%A"
    query
    |> Request.send
    |> Response.toText



[<EntryPoint>]
let main args =
    match args with
        | [||] ->
            printfn "Please supply path to your canvas token as first argument"
            1
        | [|filename|] ->
            let token = readToken filename
            sendQuery token
            |> printfn "%s"
            0
        | _ -> printfn "Yeah, don't do that m'kay"; 1
    
