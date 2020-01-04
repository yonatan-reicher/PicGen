[<AutoOpen>]
module rec PicGen.Vectors

open Vector

type XYZW = X | Y | Z | W
type vector =
    abstract Components:float list with get

type vector2 = 
    {X:float; Y:float}
    interface vector with
        override t.Components = [t.X; t.Y]
type vector3 = 
    {X:float; Y:float; Z:float}
    interface vector with
        override t.Components = [t.X; t.Y; t.Z]
type vector4 = 
    {X:float; Y:float; Z:float; W:float}
    interface vector with
        override t.Components = [t.X; t.Y; t.Z; t.W]
        
let inline (.+.) (u:'a when 'a:>vector) (v:'a) :'a = List.map2 (+) u.Components v.Components |> make
let inline (.-.) (u:'a when 'a:>vector) (v:'a) :'a = List.map2 (-) u.Components v.Components |> make
let inline (~-.) (u:'a when 'a :> vector) :'a = List.map (~-) u.Components |> make
let inline (.*.) (u:'a when 'a:>vector) (v:'a) :'a = List.map2 (*) u.Components v.Components |> make
let inline (./.) (u:'a when 'a:>vector) (v:'a) :'a = List.map2 (/) u.Components v.Components |> make
let inline ( *** ) u v = u .*. v |> components |> List.sum
let inline (+++) ({X=x1;Y=y1;Z=z1}:vector3,{X=x2;Y=y2;Z=z2}:vector3) = {X=y1*z2-z1*y2; Y=z1*x2-x1*z2; Z=x1*y2-y1*x2}
let inline (.+) (u:'a) n :'a = u |> components |> List.map ((+) n) |> make
let inline (+.) n u = u .+ n
let inline (.-) u n = u .+ (-n)
let inline (-.) n u = -.u .+ n
let inline (.*) (u:'a) n :'a = u |> components |> List.map ((*) n) |> make
let inline ( *.) n u = u .* n
let inline (./) u n = u .* (1./n)
let inline (/.) n (u:'a when 'a :> vector) :'a = List.map (fun x -> n/x) u.Components |> make

module Vector =
    let (|Components|) (u:vector) = u.Components
    let components (Components x) = x
    let make :float list->#vector = function
        | [x; y] -> {X=x; Y=y} :> vector :?> #vector
        | [x; y; z] -> {X=x; Y=y; Z=z} :> vector :?> #vector
        | x -> failwithf "Cannot create a vector out of %O" x
    let unit<'a when 'a:>vector> : 'a =
        let t =  typeof<'a>
        if t = typeof<vector2> then {X=1.;Y=1.} :> vector :?> 'a
        elif t = typeof<vector3> then {X=1.;Y=1.;Z=1.} :> vector :?> 'a
        elif t = typeof<vector4> then {X=1.;Y=1.;Z=1.;W=1.} :> vector :?> 'a
        else failwithf "Cannot get the unit vector of %O" t
    let zero<'a when 'a :> vector> = unit<'a> .* 0.
    [<AutoOpen>] 
    module Operators =
        let sqrMag (Components list) = List.map (fun x -> x*x) list |> List.sum
        let mag u = sqrt (sqrMag u)
        let x u = (components u).[0]
        let y u = (components u).[1]
        let z u = (components u).[2]
        let w u = (components u).[3]
        let get = function
            | X -> x
            | Y -> y
            | Z -> z
            | W -> w
        let get2 a b u = {X=get a u; Y=get b u}
        let get3 a b c u = {X=get a u; Y=get b u; Z=get c u}
        let get4 a b c d u = {X=get a u; Y=get b u; Z=get c u; W=get d u}
        let map f (u:'a when 'a :> vector) :'a = List.map f u.Components |> make
        let mapt2 f ({X=x;Y=y}:vector2) = (f x,f y)
        let mapt3 f ({X=x;Y=y;Z=z}:vector3) = (f x,f y,f z)
        let mapt4 f ({X=x;Y=y;Z=z;W=w}:vector4) = (f x,f y,f z,f w)
        let reduce f u = u |> components |> List.reduce f
        let normalize u = u ./ (mag u)
        let reflect (normal:'a when 'a :> vector) (reflect:'a) :'a = reflect .-. (2.*(reflect *** normal))*.normal
        let lerp (a:'a when 'a :> vector) (b:'a) t :'a = a.*(1.-t) .+. b.*t