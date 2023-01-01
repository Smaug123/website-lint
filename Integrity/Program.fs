namespace Integrity

open System
open System.Collections.Generic
open System.IO
open System.IO.Abstractions
open System.Net.Http
open System.Net.Http.Headers
open Argu

type LinkValidationError =
    | DidNotExistOnDisk
    | DidNotExistOnInternet of errorString : string

    override this.ToString () =
        match this with
        | LinkValidationError.DidNotExistOnDisk -> "(internal link)"
        | LinkValidationError.DidNotExistOnInternet e -> sprintf "(via Internet, error: %s)" e

type RelativeHtmlPath =
    | RelativeHtmlPath of string

    override this.ToString () =
        match this with
        | RelativeHtmlPath p -> p

type ArgsFragment =
    | [<ExactlyOnce ; EqualsAssignmentOrSpaced>] Website of string
    | [<ExactlyOnce ; EqualsAssignmentOrSpaced>] External_Link_File of string
    | [<ExactlyOnce ; EqualsAssignmentOrSpaced>] Root_Folder of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Website _ -> "the website you're scanning - we won't hit this, it's used to filter out local links"
            | External_Link_File _ ->
                "path to a text file containing a newline-delimited list of links to pages of the website that must be there"
            | Root_Folder _ -> "folder we'll scan, intended to be served as a static site"

type Args =
    {
        Website : string
        ExternalLinkFile : IFileInfo
        RootFolder : IDirectoryInfo
    }

module Program =

    let fetchHtml (httpClient : HttpClient) (u : Uri) : Result<string, string> Async =
        async {
            let! result = httpClient.GetAsync u |> Async.AwaitTask |> Async.Catch

            match result with
            | Choice1Of2 result ->
                if result.IsSuccessStatusCode then
                    let! content = result.Content.ReadAsStringAsync () |> Async.AwaitTask
                    return Ok content
                else
                    return Error result.ReasonPhrase
            | Choice2Of2 result -> return Error result.Message
        }

    /// Returns any broken links.
    let validateLinks<'a>
        (sleep : TimeSpan -> unit Async)
        (getLink : 'a -> Link IReadOnlyList)
        (fetchHtml : Uri -> Result<string, string> Async)
        (website : string)
        (rootFolder : IDirectoryInfo)
        (allLinks : Map<RelativeHtmlPath, 'a>)
        : (RelativeHtmlPath * (Link * LinkValidationError) list) list
        =

        let validateExternalLink = Link.validateExternal sleep fetchHtml

        let fs = rootFolder.FileSystem

        allLinks
        |> Map.toSeq
        |> Seq.choose (fun (path, links) ->
            let badLinks =
                getLink links
                |> Seq.map (fun (Link linkPath as link) ->
                    let linkPath =
                        if linkPath.StartsWith website then
                            linkPath.Substring website.Length
                        else
                            linkPath

                    if linkPath.StartsWith '/' then
                        let candidates =
                            [
                                yield linkPath
                                yield
                                    if linkPath.EndsWith '/' then
                                        $"{linkPath}index.html"
                                    else
                                        $"{linkPath}/index.html"
                            ]

                        if
                            candidates
                            |> List.exists (fun c -> Map.tryFind (RelativeHtmlPath c) allLinks |> Option.isSome)
                        then
                            // Successfully looked up this page; ignore, as it's not an error.
                            async.Return None
                        else
                            // Not an HTML file, or not found; look it up on the disk.
                            let f =
                                fs.Path.Combine (rootFolder.FullName, linkPath.TrimStart '/')
                                |> fs.FileInfo.FromFileName

                            if f.Exists then
                                async.Return None
                            else
                                Some (link, DidNotExistOnDisk) |> async.Return
                    elif linkPath.[0] = '#' then
                        // An anchor link
                        // TODO: make sure the anchor exists
                        None |> async.Return
                    else
                        // An external link!
                        let uri =
                            try
                                Uri linkPath
                            with e ->
                                eprintfn "%s" linkPath
                                reraise ()

                        let unsupported = Set.ofList [ "mailto" ]

                        if unsupported |> Set.contains uri.Scheme then
                            async.Return None
                        else

                            async {
                                match! validateExternalLink uri with
                                | Ok _ -> return None
                                | Error e -> return Some (link, DidNotExistOnInternet e)
                            }
                )
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Seq.choose id
                |> Seq.toList

            match badLinks with
            | [] -> None
            | badLinks -> Some (path, badLinks)
        )
        |> Seq.toList

    [<EntryPoint>]
    let main argv =
        let fs = FileSystem ()

        let parser = ArgumentParser.Create<ArgsFragment> ()
        let parsed = parser.Parse argv

        let args =
            {
                Website = parsed.GetResult ArgsFragment.Website
                ExternalLinkFile = parsed.GetResult ArgsFragment.External_Link_File |> fs.FileInfo.FromFileName
                RootFolder = parsed.GetResult ArgsFragment.Root_Folder |> fs.DirectoryInfo.FromDirectoryName
            }

        let allLinks =
            args.RootFolder.EnumerateFiles ("*.html", SearchOption.AllDirectories)
            |> Seq.map (fun file ->
                let name = file.FullName.Substring (args.RootFolder.FullName.Length - 1)
                RelativeHtmlPath name, Link.lintFile file
            )
            |> Map.ofSeq

        let whoHasLinkedToUs = args.ExternalLinkFile.FullName |> fs.File.ReadAllLines

        let weHaveBrokenThirdParties =
            whoHasLinkedToUs
            |> Seq.choose (fun link ->
                if not <| link.StartsWith args.Website then
                    failwith "they linked to a different website?"

                let link = link.Substring (args.Website.Length + 1)
                let link = if link.EndsWith '/' then $"{link}/index.html" else link

                let f = fs.Path.Combine (args.RootFolder.FullName, link) |> fs.FileInfo.FromFileName
                if f.Exists then None else Some (RelativeHtmlPath link)
            )
            |> Seq.toList

        for broken in weHaveBrokenThirdParties do
            eprintfn $"We have broken a third-party link to {broken}"

        use httpClient = new HttpClient ()

        [
            "Mozilla/5.0"
            "(Macintosh; Intel Mac OS X 10_15_7)"
            "AppleWebKit/537.36"
            "(KHTML, like Gecko)"
            "Chrome/104.0.0.0"
            "Safari/537.36"
        ]
        |> List.iter (ProductInfoHeaderValue.Parse >> httpClient.DefaultRequestHeaders.UserAgent.Add)

        // Check internal links
        let nonExistentInternalLinks =
            validateLinks Async.Sleep fst (fetchHtml httpClient) args.Website args.RootFolder allLinks
            |> Seq.collect (fun (localLink, errors) ->
                errors
                |> List.map (fun (link, error) -> link, error, localLink)
                |> List.groupBy (fun (link, _, _) -> link)
                |> Seq.map (fun (link, errors) -> link, errors |> List.map (fun (_, error, path) -> error, path))
            )
            |> Map.ofSeq

        for KeyValue (Link url, links) in nonExistentInternalLinks do
            links
            |> Seq.map (fun (error, RelativeHtmlPath localPath) ->
                sprintf "%s %s" localPath (string<LinkValidationError> error)
            )
            |> String.concat "\n  "
            |> eprintfn "The following links to %s did not resolve:\n  %s" url

        0
