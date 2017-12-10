namespace global

open System
open System.Collections.Generic
open System.Text


type BitList () =
    let list = List ()
    let mutable currByte = 0uy
    let mutable bitCount = 0

    member x.Push bit =
        currByte <- (currByte <<< 1) ||| (if bit then 1uy else 0uy)
        bitCount <- bitCount + 1
        if bitCount = 8 then
            list.Add currByte
            currByte <- 0uy
            bitCount <- 0

    member x.PushRange (bit, count) =
        for i in 0 .. count - 1 do
            x.Push bit

    member x.Flush () =
        if bitCount > 0 then
            let newCurrByte = currByte <<< (8 - bitCount)
            list.Add newCurrByte
        let result = list.ToArray ()
        list.Clear ()
        bitCount <- 0
        result

type BitArrayReader (bytes : byte []) =
    let mutable currPos = 0

    member x.BytesCount = bytes.Length

    member x.Peek () =
        if currPos / 8 < bytes.Length then
            (bytes.[currPos / 8] >>> (7 - (currPos % 8))) &&& 1uy <> 0uy
        else
            false

    member x.Read () =
        let result = x.Peek ()
        currPos <- currPos + 1
        result

    member x.ReadMulti count =   // need more optimized version
        let rec read count acc =
            if count <= 0 then acc
            else read (count - 1) ((acc <<< 1) ||| (if x.Read () then 1 else 0))
        read count 0

type RangeCoder (extentPrecision) =
    let extent = 1 <<< extentPrecision
    let bitList = BitList ()
    let mutable currLo = 0
    let mutable currHi = extent
    let mutable currBitsAcc = 0

    member x.Extent = extent
    member x.CurrentExtent = currHi - currLo
    member x.CurrentHigh = currHi
    member x.CurrentLow = currLo

    member x.Push localExtent localLo localHi =
        let lo = int (int64 localLo * int64 (currHi - currLo) / int64 localExtent) + currLo
        let hi = int (int64 localHi * int64 (currHi - currLo) / int64 localExtent) + currLo
        if lo = hi then
            raise (InvalidOperationException ("Zero range"))
        let rec popBits lo hi bitsAcc =
            if hi <= extent / 2 then
                bitList.Push false
                bitList.PushRange (true, bitsAcc)
                popBits (lo * 2) (hi * 2) 0
            elif lo >= extent / 2 then
                bitList.Push true
                bitList.PushRange (false, bitsAcc)
                popBits (lo * 2 - extent) (hi * 2 - extent) 0
            elif lo >= extent / 4 && hi <= extent * 3 / 4 then
                popBits (lo * 2 - extent / 2) (hi * 2 - extent / 2) (bitsAcc + 1)
            else
                currLo <- lo
                currHi <- hi
                currBitsAcc <- bitsAcc
        popBits lo hi currBitsAcc

    member x.PushRange (rangeList : MultiRangeList) index =
        let range = rangeList.GetRange index
        x.Push rangeList.Extent range.Start range.End

    member x.Flush () =
        bitList.Push true
        currLo <- 0
        currHi <- extent
        currBitsAcc <- 0
        bitList.Flush ()

type RangeDecoder (extentPrecision) =
    let extent = 1 <<< extentPrecision
    let mutable currLo = 0
    let mutable currHi = extent

    member x.Extent = extent
    member x.CurrentExtent = currHi - currLo
    member x.CurrentHigh = currHi
    member x.CurrentLow = currLo

    member x.Push localExtent localLo localHi code =
        let lo = int (int64 localLo * int64 (currHi - currLo) / int64 localExtent) + currLo
        let hi = int (int64 localHi * int64 (currHi - currLo) / int64 localExtent) + currLo
        let rec popBits lo hi code bitsPushed =
            let hiRegion = hi >>> (extentPrecision - 2)
            let loRegion = lo >>> (extentPrecision - 2)
            if ((hi - 1) >>> (extentPrecision - 2)) - (lo >>> (extentPrecision - 2)) < 2 then
                if (code >>> (extentPrecision - 1)) &&& 1 = 0 then
                    popBits (lo * 2) (hi * 2) (code <<< 1) (bitsPushed + 1)
                else
                    popBits (lo * 2 - extent) (hi * 2 - extent) (code <<< 1) (bitsPushed + 1)
            else
                currLo <- lo
                currHi <- hi
                bitsPushed
        popBits lo hi code 0

    member x.PushRange (rangeList : MultiRangeList) index code =
        let range = rangeList.GetRange index
        x.Push rangeList.Extent range.Start range.End code



