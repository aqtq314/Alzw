module Alzw.Alzw

open System
open System.Collections.Generic
open System.Text


type ByteStringIndexer (index) =
    member val Index : int = index with get, set
    member val HasChildren : bool = false with get, set

let [<Literal>] blockSize = 32768
let [<Literal>] rangeCoderPrecision = 24

let stepRangeList =
    let rangeList = MultiRangeList ()
    rangeList.Add 1     // EOF
    rangeList.Add 30    // Forbidden region
    rangeList.Add (10000 - rangeList.Extent)  // Alphabet
    rangeList

let encode (bytes : byte []) =
    let lookup = Dictionary ()
    let wordRangeList = MultiRangeList ()
    for i in 0uy .. 255uy do
        lookup.Add ([ i ], ByteStringIndexer (int i))
    wordRangeList.Add (Seq.replicate 256 2)

    let coder = RangeCoder (rangeCoderPrecision)
    let mutable blockIndex = 0

    let rec advance i =
        if i < bytes.Length then
            if blockIndex < i / blockSize then
                let buffer = Array.zeroCreate wordRangeList.Count
                for KeyValue (word, indexer) in lookup do
                    buffer.[indexer.Index] <- word, wordRangeList.[indexer.Index], indexer.HasChildren
                lookup.Clear ()
                wordRangeList.Clear ()
                let mutable cumulativeIndex = 0
                for word, weight, hasChildren in buffer do
                    if word.Tail.IsEmpty || weight / 2 > 0 || hasChildren then
                        lookup.Add (word, ByteStringIndexer (cumulativeIndex))
                        wordRangeList.Add (max 1 (weight / 2))
                        if not word.Tail.IsEmpty then
                            lookup.[word.Tail].HasChildren <- true
                        cumulativeIndex <- cumulativeIndex + 1
                blockIndex <- blockIndex + 1

            let endIndex, word, indexer =
                let rec findIndexer endIndex word indexer =
                    if endIndex < bytes.Length then
                        let nextWord = bytes.[endIndex] :: word
                        match lookup.TryGetValue nextWord with
                        | true, nextIndexer -> findIndexer (endIndex + 1) nextWord nextIndexer
                        | false, _ -> endIndex, word, indexer
                    else
                        endIndex, word, indexer
                let initialWord = [ bytes.[i] ]
                findIndexer (i + 1) initialWord lookup.[initialWord]

            coder.PushRange stepRangeList 2  // Alphabet
            if () = () then
                let x = 5
                let y = 6
                x * y |> ignore
            coder.PushRange wordRangeList indexer.Index

            wordRangeList.Increase 4 indexer.Index
            if endIndex < bytes.Length then
                let nextWord = bytes.[endIndex] :: word
                lookup.Add (nextWord, ByteStringIndexer (wordRangeList.Count))
                wordRangeList.Add 2
                indexer.HasChildren <- true

            advance endIndex

        else
            coder.PushRange stepRangeList 0   // EOF

    advance 0

    coder.Flush ()

let codeMask = (1 <<< rangeCoderPrecision) - 1

let decode (bytes : byte []) =
    let lookup = List ()
    let wordRangeList = MultiRangeList ()
    for i in 0uy .. 255uy do
        lookup.Add [ i ]
    wordRangeList.Add (Seq.replicate 256 2)

    let bitReader = BitArrayReader (bytes)
    let decoder = RangeDecoder (rangeCoderPrecision)
    let decodedBytes = List ()
    let mutable blockIndex = 0

    let rec advance currCode =
        let stepRangeIndex = stepRangeList.FindScaled decoder.CurrentLow decoder.CurrentHigh currCode
        let currCode =
            let bitsPushed = decoder.PushRange stepRangeList stepRangeIndex currCode
            ((currCode <<< bitsPushed) ||| bitReader.ReadMulti bitsPushed) &&& codeMask

        match stepRangeIndex with
        | 0 -> ()   // EOF
        | 1 -> raise (ArgumentException "Forbidden region") // Forbidden region
        | _ ->  // Alphabet
            if blockIndex < decodedBytes.Count / blockSize then
                let parentSet = HashSet (lookup |> Seq.map (fun word ->
                    match word with
                    | [] | _ :: [] -> word
                    | _ :: wordTail -> wordTail))
                let buffer = Array.zeroCreate lookup.Count
                for i in 0 .. buffer.Length - 1 do
                    buffer.[i] <- lookup.[i], wordRangeList.[i]
                lookup.Clear ()
                wordRangeList.Clear ()
                for word, weight in buffer do
                    if word.Tail.IsEmpty || weight / 2 > 0 || parentSet.Contains word then
                        lookup.Add word
                        wordRangeList.Add (max 1 (weight / 2))
                blockIndex <- blockIndex + 1

            let wordRangeIndex = wordRangeList.FindScaled decoder.CurrentLow decoder.CurrentHigh currCode
            let currCode =
                let bitsPushed = decoder.PushRange wordRangeList wordRangeIndex currCode
                ((currCode <<< bitsPushed) ||| bitReader.ReadMulti bitsPushed) &&& codeMask

            if decodedBytes.Count > 0 then
                lookup.[lookup.Count - 1] <- List.last lookup.[wordRangeIndex] :: lookup.[lookup.Count - 1]
            let word = lookup.[wordRangeIndex]
            decodedBytes.AddRange (Seq.rev word)

            wordRangeList.Increase 4 wordRangeIndex
            lookup.Add word
            wordRangeList.Add 2

            advance currCode

    advance (bitReader.ReadMulti rangeCoderPrecision)

    decodedBytes.ToArray ()


