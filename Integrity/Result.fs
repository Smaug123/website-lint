namespace Integrity

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Result =

    let partition<'a, 'b> (l : Result<'a, 'b> seq) : 'a IReadOnlyList * 'b IReadOnlyList =
        let oks = ResizeArray<'a> ()
        let errors = ResizeArray<'b> ()

        for result in l do
            match result with
            | Ok a -> oks.Add a
            | Error b -> errors.Add b

        oks, errors
