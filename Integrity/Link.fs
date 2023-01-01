namespace Integrity

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO.Abstractions
open System.Threading
open System.Threading.Tasks
open HtmlAgilityPack

type LinkError = | NoHref of IFileInfo * HtmlNode

type Link = | Link of string

[<RequireQualifiedAccess>]
module Link =

    let lintFile (file : IFileInfo) : Link IReadOnlyList * LinkError IReadOnlyList =
        let fs = file.FileSystem
        let doc = HtmlDocument ()
        fs.File.ReadAllText file.FullName |> doc.LoadHtml

        match doc.DocumentNode with
        | null -> failwith $"Unexpectedly got a null DocumentNode on {file.FullName}"
        | doc ->

            if isNull (doc.SelectNodes "//body") then
                [], []

            else

                match doc.SelectNodes "//a[@href]" with
                | null ->
                    // why would you give `null` when what you really mean is an empty collection :(
                    [], []
                | nodes ->

                    nodes
                    |> Seq.choose (fun node ->
                        match node.Attributes with
                        | null -> failwith "Unexpectedly got no attributes on node"

                        | attrs ->

                            match attrs.["href"] with
                            | null -> Error (NoHref (file, node)) |> Some
                            | v -> Ok (Link v.Value) |> Some
                    )
                    |> Result.partition

    type private Fetcher<'a> =
        | Claimed of int
        | WaitFor of Task<'a>

    let validateExternal
        (sleep : TimeSpan -> unit Async)
        (fetchHtml : Uri -> Result<string, string> Async)
        : Uri -> Async<Result<string, string>>
        =
        let store = ConcurrentDictionary ()
        let counter = ref 0

        let forceSuccess =
            // These websites are flaky or slow, but we assume I've got them right.
            [
                "web.archive.org", "slow to access"
                "news.ycombinator.com", "flaky and forbids access"
                "www.linkedin.com", "forbids access"
            ]
            |> Map.ofList

        let rec get (uri : Uri) =
            match Map.tryFind uri.Host forceSuccess with
            | Some reason -> async.Return (Ok $"%s{uri.Host}: %s{reason}")
            | None ->

                async {
                    let me = Interlocked.Increment counter

                    let added =
                        store.GetOrAdd (uri, Func<_, _, _> (fun uri () -> Fetcher.Claimed me), ())

                    match added with
                    | Fetcher.Claimed i when i = me ->
                        let running = fetchHtml uri |> Async.StartAsTask
                        store.[uri] <- WaitFor running
                        let! result = running |> Async.AwaitTask

                        match result with
                        | Error e -> Console.WriteLine $"Error fetching {uri}: {e}"
                        | Ok _ -> ()

                        return result
                    | Fetcher.Claimed _ ->
                        do! sleep (TimeSpan.FromMilliseconds 50.0)
                        return! get uri
                    | Fetcher.WaitFor t -> return! t |> Async.AwaitTask
                }

        get
