module PicGen.Raymarching.Scenes

open PicGen
open PicGen.Raymarching
open Vector

type [<ReferenceEquality>] sceneObject =
    {   Color:vector3->color
        Reflection:float
        Roughness:float }
module sceneObject =
    let singleColor color reflection roughness = 
        {Color=(fun _ -> color); Reflection=reflection; Roughness=roughness}
    let checkerBoard color1 color2 reflection roughness = 
        let f p = 
            let sum = int (reduce (+) p)
            if sum % 2 = 0 then color1
            else                color2
        {Color=f; Reflection=reflection; Roughness=roughness}

type field3d = field<vector3,sceneObject>

type renderConfig =
    {   //  Lighting 
        Reflections:int
        LightPosition:vector3
        Skybox:vector3->color
        //  RayMarching
        RayIterations:int
        RayDirection:vector3
        RayOrigin:vector3
        SurfaceDistance:float
        MaxRayDistance:float }

let raymarch ({RayOrigin=ro; RayDirection=rd; SurfaceDistance=surface; RayIterations=i; MaxRayDistance=maxDistance} as config:renderConfig) scene = Field.raymarch ro rd surface i maxDistance scene

///<summary>Returns the lighting of the object and the reflected color seperetly</summary>
let rec light ({LightPosition=lp
                Reflections=reflections
                RayDirection=rd
                SurfaceDistance=surface} as config) p field =
    let dir = lp .-. p
    let dirMag = mag dir
    let dirNormalized = dir./dirMag
    let normal = Field.normal 0.02 p field
    let p' = p .+. 2.*.normal.*surface
    let reflectedRd = reflect normal rd
    let r = 
        if reflections > 0 then 
            let config' = {config with  Reflections=reflections-1
                                        RayDirection=reflectedRd
                                        RayOrigin=p'    }
            color config' field
        else unit
    match raymarch {config with MaxRayDistance = dirMag; RayDirection=dirNormalized; RayOrigin=p'} field with
    | Error _ -> 
        let l = (reflectedRd *** dirNormalized) + 0.2
        max 0. l
    | _ -> 0.
    , r
    
and color ({RayDirection=rd; RayOrigin=ro; Skybox=skybox} as config) field =
    match raymarch config field with
    | Ok (d,{Color=colorMap; Reflection=reflective; Roughness=roughness},_) ->
        let p = ro .+. d*.rd
        let color = colorMap p
        let l, r = light config p field
        let l' = unit.*l .+. r.*reflective
        let smooth = l'.*.color
        let rough = color ./ 2.
        let color' = lerp smooth rough roughness
        color' |> map (max 0.) |> map (min 1.)
    | Error _ -> skybox rd
    
let frag ({RayDirection = rd} as config) scene (uv:vector2) = 
    let uv' = uv.*2. .- 1.
    let right = normalize ({X=0.;Y=1.;Z=0.} +++ rd)
    let up = normalize (rd +++ right)
    let rd = normalize (rd .+. right.*uv'.X .+. up.*uv'.Y)
    color {config with RayDirection=rd} scene