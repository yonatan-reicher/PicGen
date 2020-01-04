[<AutoOpen>]
module PicGen.Canvas

type canvas = Canvas of vector3 [][]
type color = vector3

module Canvas =
    open Vector

    let width (Canvas a) = a.[0].Length
    let height (Canvas a) = a.Length
    let fragment width height f = 
        let fragmentRowAsync y = [|for x in 0..width-1 -> f x (height-1-y) |> map (max 0.)|]
        let pixels = 
            [for y in 0..height-1 -> async { return fragmentRowAsync y }]
            |> Async.Parallel 
        async {
            let! pixels = pixels
            return Canvas pixels
        }
    let fragmentNormalized width height f = 
        let div = max width height |> float
        fragment width height (fun x y -> f {X=float x / div; Y=float y / div})
    let empty width height = fragment width height (fun _ _ -> zero)
    let set x y value (Canvas a) = a.[y].[x] <- value
    let get x y (Canvas a) = a.[y].[x]
    let toPPM (Canvas arr as c) =
        let stringVec (x:vector3) = x .* 255. |> fun ({X=x;Y=y;Z=z}:vector3) -> sprintf " %d %d %d" (int x) (int y) (int z)
        arr
        |> Array.map (Array.map stringVec >> String.concat " ")
        |> String.concat "\n"
        |> sprintf "P3\n%d %d\n255\n%s" (width c) (height c)