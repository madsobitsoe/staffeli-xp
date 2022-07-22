namespace Parsers

open FSharp.Core
open System

module Parser =

    type Parser<'a> = Parser of (string -> Result<'a*string,string>)

    let runParser (Parser p) = p

    let fail<'a> = Parser <| fun s -> Error "failed"

    type ResultBuilder() =
        member __.Return x = Ok x
        member __.ReturnFrom m = m
        member __.Bind (m, f) = Result.bind f m

    let res = new ResultBuilder()

    let return_ x = Parser <| fun s -> Ok <| (x, s)

    let bind (Parser p) f = Parser <| fun s ->
            res {
                let! (p', s') = p s
                return! runParser (f p') s'
            }

    type ParserBuilder() =
        member __.Return x = return_ x
        member __.ReturnFrom x = x
        member __.Bind (p, f) = bind p f

    let parser = ParserBuilder()
    let (>>=) p f = bind p f

    let (<|>) (Parser a) (Parser b) = Parser <| fun s ->
            match a s with
            | Ok p -> Ok p
            | Error e -> b s

    let fmap f t = t >>= (f >> return_)
    let (|>>) t f = fmap f t

    let (.>>.) p1 p2 = parser {
            let! p1' = p1
            let! p2' = p2
            return (p1', p2')
        }
    let (.<<) p1 p2 = p1 .>>. p2 |>> fst
    let (.>>) p1 p2 = p1 .>>. p2 |>> snd

    let opt p = fmap Some p <|> (return_ None)

    let many p =
        let p' = opt p
        let rec go xs = p' >>= function Some x -> go (x::xs)
                                      | None _ -> return_ <| List.rev xs
        go []

    let many1 p = parser {
            let! (x,xs) = (p .>>. many p)
            return (x::xs)
        }

    let sepBy p sep = parser {
            let! (x,xs) = (p .>>. many (sep .>> p))
            return (x::xs)
        }

    let satisfy pred = Parser <| fun s ->
            if String.length s = 0 || not <| pred s.[0] then Error "failed"
            else Ok <| (s.[0], s[1..])

    let choice = List.reduce (<|>)
    let anyChar = satisfy (fun _ -> true)
    let pchar c = satisfy ((=) c)
    let anyOf lst =
        lst |> List.map pchar |> choice
    let letter = anyOf <| List.append ['a' .. 'z'] ['A'..'Z']
    let digit = anyOf ['0'..'9']

    let between l r inn = (l .>> inn) .<< r

    let pint =
        let go acc el = 10*acc + int el - int '0'
        many digit >>= function
            | [] -> fail
            | xs -> return_ <| List.fold go 0 xs

module InputParser =
    let empty = 1

    type Task = {
        Title: string
        PointMax: Option<int>
        Points: Option<int>
        FeedBack: string
    }

    type Assignment =
        { Name: string
          PassingPoints: Option<int>
          ShowPoints: bool
          TA: Option<string>
          Tasks: List<Task> }

          static member Default =
              { Name = ""
                PassingPoints = None
                ShowPoints = false
                TA = None
                Tasks = [] }
