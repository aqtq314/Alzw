namespace global

open System
open System.Collections.Generic
open System.Text


[<Struct>]
type Range =
    val Start : int
    val Length : int
    new (start, length) = { Start = start; Length = length }
    member x.End = x.Start + x.Length
    override x.ToString () = sprintf "[%d, %d)" x.Start x.End
    static member ofStartEnd start ``end`` = Range (start, ``end`` - start)

type MultiRangeList () =
    let layers = List ()
    do layers.Add (Array.zeroCreate 1)
    let mutable count = 0
    let mutable extent = 0

    member x.Count = count
    member x.Extent = extent
    member x.LayersCount = layers.Count

    member x.Print () =
        let strbs = Array.init layers.Count (fun _ -> StringBuilder ())
        let lastLayer = layers.[layers.Count - 1]
        let lastStrb = strbs.[strbs.Length - 1]
        for i in 0 .. lastLayer.Length - 1 do
            let valueStr =
                if i < count then string lastLayer.[i]
                else sprintf "[%d]" lastLayer.[i]
            lastStrb.Append(' ').Append(valueStr) |> ignore
            let mutable i2 = i
            let mutable layerIndex = layers.Count - 2
            while (i2 &&& 1) <> 0 do
                i2 <- i2 >>> 1
                let valueStr =
                    if i < count then string layers.[layerIndex].[i2]
                    else sprintf "[%d]" layers.[layerIndex].[i2]
                strbs.[layerIndex].Append(' ', lastStrb.Length - strbs.[layerIndex].Length - valueStr.Length).Append(valueStr) |> ignore
                layerIndex <- layerIndex - 1
        strbs
        |> Seq.map (sprintf "%O")
        |> String.concat Environment.NewLine

    member private x.UpdatePSum startIndex length =
        let rec updatePSum shiftAmount =
            if shiftAmount < layers.Count then
                let fromIndex = startIndex >>> shiftAmount
                let toIndex = (startIndex + length - 1) >>> shiftAmount
                let maxToIndex = (count >>> shiftAmount) - 1
                let layer = layers.[layers.Count - shiftAmount - 1]
                let sublayer = layers.[layers.Count - shiftAmount]
                for i in fromIndex .. min toIndex maxToIndex do
                    layer.[i] <- sublayer.[i <<< 1] + sublayer.[(i <<< 1) + 1]
                updatePSum (shiftAmount + 1)
        updatePSum 1

        let rec computeExtent count layerIndex acc =
            if count <= 0 then
                acc
            elif (count &&& 1) <> 0 then
                computeExtent (count >>> 1) (layerIndex - 1) (acc + layers.[layerIndex].[count - 1])
            else
                computeExtent (count >>> 1) (layerIndex - 1) acc
        extent <- computeExtent count (layers.Count - 1) 0

    member x.Add items =
        let startIndex = count
        let newItemsCount =
            match items : seq<int> with
            | :? ICollection<int> as items -> items.Count
            | :? IReadOnlyCollection<int> as items -> items.Count
            | _ -> Seq.length items

        // create new layers
        let mutable newLayersCount = 0
        while ((startIndex + newItemsCount) >>> layers.Count) > 0 do
            layers.Add (Array.zeroCreate ((1 <<< (layers.Count + 1)) - 1))
            newLayersCount <- newLayersCount + 1

        // copy old items & psums
        if newLayersCount > 0 then
            for li in 0 .. layers.Count - newLayersCount - 1 do
                let newLayer = layers.[layers.Count - li - 1]
                let oldLayer = layers.[layers.Count - li - newLayersCount - 1]
                for i in 0 .. (startIndex >>> li) - 1 do
                    newLayer.[i] <- oldLayer.[i]

        // copy new items
        match items with
        | :? (int []) as items ->
            Array.Copy (items, 0, layers.[layers.Count - 1], startIndex, items.Length)
        | _ ->
            let newLayer = layers.[layers.Count - 1]
            let mutable localCount = startIndex
            for item in items do
                newLayer.[localCount] <- item
                localCount <- localCount + 1

        count <- startIndex + newItemsCount
        x.UpdatePSum startIndex newItemsCount

    member x.Add item =
        x.Add (Seq.singleton item)

    member x.Item
        with get i =
            if i < 0 || i >= count then
                raise (ArgumentOutOfRangeException ())
            layers.[layers.Count - 1].[i]
        and set i value =
            if i < 0 || i >= count then
                raise (ArgumentOutOfRangeException ())
            layers.[layers.Count - 1].[i] <- value
            x.UpdatePSum i 1

    member x.GetRange i =
        if i < 0 || i >= count then
            raise (ArgumentOutOfRangeException ())
        let rec computeOffset i layerIndex acc =
            if i <= 0 then
                acc
            elif (i &&& 1) <> 0 then
                computeOffset (i >>> 1) (layerIndex - 1) (acc + layers.[layerIndex].[i - 1])
            else
                computeOffset (i >>> 1) (layerIndex - 1) acc

        let start = computeOffset i (layers.Count - 1) 0
        Range (start, layers.[layers.Count - 1].[i])

    member x.Find value =
        if value < 0 || value >= extent then
            raise (ArgumentOutOfRangeException ())
        else
            let count = count
            let layersCount = layers.Count
            let rec find i layerIndex psum =
                if layerIndex >= layersCount then
                    i >>> 1
                elif i >= (count >>> (layersCount - layerIndex - 1)) then
                    find (i <<< 1) (layerIndex + 1) psum
                else
                    let newPSum = psum + layers.[layerIndex].[i]
                    if value < newPSum then
                        find (i <<< 1) (layerIndex + 1) psum
                    else
                        find ((i + 1) <<< 1) (layerIndex + 1) newPSum
            find 0 0 0

    member x.FindScaled coderLo coderHi coderValue =
        let toCoderScale value =
            int (int64 value * int64 (coderHi - coderLo) / int64 extent) + coderLo
        if coderValue < coderLo || coderValue >= coderHi then
            raise (ArgumentOutOfRangeException ())
        else
            let count = count
            let layersCount = layers.Count
            let rec find i layerIndex psum =
                if layerIndex >= layersCount then
                    i >>> 1
                elif i >= (count >>> (layersCount - layerIndex - 1)) then
                    find (i <<< 1) (layerIndex + 1) psum
                else
                    let newPSum = psum + layers.[layerIndex].[i]
                    if coderValue < toCoderScale newPSum then
                        find (i <<< 1) (layerIndex + 1) psum
                    else
                        find ((i + 1) <<< 1) (layerIndex + 1) newPSum
            find 0 0 0

    member x.Increase amount i =
        x.[i] <- x.[i] + amount

    member x.Clear () =
        count <- 0
        extent <- 0

    static member ofSeq (items : seq<_>) =
        let rangeList = MultiRangeList ()
        rangeList.Add items
        rangeList

    static member generateDiscreteValues extent count ratio =
        let extentf = float extent
        let rec generate depth acc psum = seq {
            if depth >= count then
                yield extent - psum
            else
                let nextAcc = acc * ratio
                let delta = int (extentf * acc) - int (extentf * nextAcc)
                if delta <= 0 then
                    raise (ArgumentOutOfRangeException (sprintf "delta <= 0, args: %d %d %A" extent count ratio))
                yield delta
                yield! generate (depth + 1) nextAcc (psum + delta) }
        generate 0 1.0 0
        |> MultiRangeList.ofSeq


