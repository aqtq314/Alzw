[<AutoOpen>]
module internal Util

open System


let (|>!) a f = f a; a

let rec fix f = f (lazy fix f)

