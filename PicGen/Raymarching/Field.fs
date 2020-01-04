[<AutoOpen>]
module PicGen.Raymarching.Field

open PicGen

type field<'vec,'id> when 'vec :> vector = Field of ('vec->float*'id)

module rec Field =
    open Vector

    let sample p (Field field) = field p
    let distance p field = sample p field |> fst
    let id p field = sample p field |> snd
    let sphere id radius = Field (fun p -> mag p - radius, id)
    let box id dimensions = Field (fun p ->
        let q = map abs p .-. dimensions
        mag (map (max 0.) q) + min 0. (reduce max q), id)
    let plane id = Field (fun p -> y p, id)
    
    let normal sampleDistance (p:'vec when 'vec :> vector) (field:field<'vec,_>) :'vec =
        let g p = distance p field
        let d = g p 
        //{   X = g (p .+. get3 X Y Y e) - d;
        //    Y = g (p .+. get3 Y X Y e) - d;
        //    Z = g (p .+. get3 Y Y X e) - d  }
        let dimensions = p.Components.Length
        let normalComponent i = 
            let delta = make (List.init dimensions (fun i' -> if i = i' then sampleDistance else 0.))
            g (p .+. delta) - d 
        make <| List.init dimensions normalComponent
        |> normalize
        
    let rec raymarch ro rd surface maxIter maxDistance field =
        let distance, id = sample ro field 
        if abs distance < surface then Ok (distance, id, maxIter)
        elif maxIter = 0 || distance > maxDistance then Error (distance, maxIter)
        else 
            match raymarch (ro .+. distance*.rd) rd surface (maxIter-1) maxDistance field with
            | Ok (distance', id', maxIter') -> Ok (distance+distance', id', maxIter')
            | Error (distance', maxIter') -> Error (distance+distance', maxIter')

    [<AutoOpen>]
    module Operators =
        let union f1 f2 = Field (fun p ->
            let d1, i1 = sample p f1
            let d2, i2 = sample p f2
            if d1 < d2 then d1, i1
            else            d2, i2)
        let intersect f1 f2 = Field (fun p ->
            let d1, i1 = sample p f1
            let d2, i2 = sample p f2
            max d1 d2, i1)
        let inverse f1 = Field (fun p ->
            let d, i = sample p f1
            -d, i)
        let remove f1 f2 = intersect f1 (inverse f2)
        let weld id f1 = Field (fun p -> distance p f1, id)

        let translate amount field = Field (fun p -> sample (p.-.amount) field)
        let scale dimesions field = Field (fun p ->
            let d, i = sample (p./.dimesions) field
            d * (reduce max dimesions), i)
