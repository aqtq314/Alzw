module Program

open Alzw
open System
open System.Collections.Generic
open System.Text


let [<Literal>] defaultBlockSize = 32768
let [<Literal>] defaultForbiddenRegion = 30

let printUsage () =
    let filename = System.IO.Path.GetFileName ((Environment.GetCommandLineArgs ()).[0]);
    eprintfn """Usage:"""
    eprintfn """    %s (enc|dec) inputFile outputFile [-b blockSize] [-e forbiddenRegion]""" filename
    eprintfn ""
    eprintfn "%s" <| """blockSize: Number of bytes processed before each forget-routine. """ +
        """Must be an integer between [1024, 16777216]. Default is 32768."""
    eprintfn ""
    eprintfn "%s" <| """forbiddenRegion: Size of the forbidden region over 10000. """ +
        """Must be an integer between [1, 9998] (need 1 for EOF and at least 1 for the alphabets). """ +
        """Default is 30."""
    eprintfn ""

[<EntryPoint>]
let main argv =
    let (|IsEnc|_|) mode =
        if String.Equals (mode, "enc", StringComparison.CurrentCultureIgnoreCase) then
            Some true
        elif String.Equals (mode, "dec", StringComparison.CurrentCultureIgnoreCase) then
            Some false
        else
            None

    let (|Int|_|) minValue maxValue value =
        match Int32.TryParse value with
        | true, value when value >= minValue && value <= maxValue -> Some value
        | _, _ -> None

    let (|Options|_|) options =
        match options with
        | "-b" :: Int 1024 16777216 blockSize :: "-e" :: Int 1 9998 forbiddenRegion :: [] ->
            Some (blockSize, forbiddenRegion)
        | "-b" :: Int 1024 16777216 blockSize :: [] ->
            Some (blockSize, defaultForbiddenRegion)
        | "-e" :: Int 1 9998 forbiddenRegion :: [] ->
            Some (defaultBlockSize, forbiddenRegion)
        | [] ->
            Some (defaultBlockSize, defaultForbiddenRegion)
        | _ ->
            None

    match List.ofArray argv with
    | IsEnc isEnc :: inputFile :: outputFile :: Options (blockSize, forbiddenRegion) ->
        let bytes = System.IO.File.ReadAllBytes inputFile
        let coder = AlzwCoder (blockSize, forbiddenRegion)
        let outBytes =
            if isEnc
            then coder.Encode bytes
            else coder.Decode bytes
        System.IO.File.WriteAllBytes (outputFile, outBytes)

    | _ ->
        printUsage ()

    0
