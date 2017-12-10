module Program

open Alzw
open System
open System.Collections.Generic
open System.Text


//let str = @"A quantifier specifies how many instances of the previous element (which can be a character, a group, or a character class) must be present in the input string for a match to occur. Quantifiers include the language elements listed in the following table. For more information, see Quantifiers."
let str = System.IO.File.ReadAllText @"C:\Users\User\Desktop\bible.50w.txt"
//let str = System.IO.File.ReadAllText @"C:\Users\User\Desktop\bible.32768.txt"

[<EntryPoint>]
let main argv =
    //"aaaaaaaaaaa"
    str
    |> Encoding.Default.GetBytes
    |> Alzw.encode
    |>! fun encodedBytes -> printfn "Encoded bytes count: %d" encodedBytes.Length
    |> Alzw.decode
    |> Encoding.Default.GetString
    //|> printfn "Decoded string: %s"
    |> fun newstr -> printfn "Are results equal: %b" (str = newstr)

    Console.ReadKey () |> ignore
    0
