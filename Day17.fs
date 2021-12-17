module AoC2021.Day17

open AoC2021.Utils

let checkTarget x0 y0 x1 y1 x y =
    if x0 <= x && y0 <= y && x <= x1 && y <= y1 then
        Some true
    else if x < x1 &&  y > y1 then
        Some false
    else
        None


let trajectory target (velX, velY, x, y) =
    let x' = x + velX
    let y' = y + velY

    let velX' =
        match velX with
        | 0 -> 0
        | n -> n - 1
        // We can ignore negative X as it will never go towards the target zone.

    let velY' = velY - 1

    match target x' y' with
    | Some false -> Some((false, x', y'), (velX', velY', x', y'))
    | Some true -> Some((true, x', y'), (velX', velY', x', y'))
    | None -> None

let shoot target velX velY =
    Seq.unfold (trajectory target) (velX, velY, 0, 0)
   
let isHit s =
    match s|> Seq.exists (fun (h,_,_) -> h) with
    | true -> Some (s |> Seq.map (fun (_, _, y) -> y)|> Seq.max)
    | false -> None

let getParameters x0 y0 x1 y1 =
   let target = checkTarget x0 y0 x1 y1
   let launch (x,y) = shoot target x y 
   let potentialXvs = [1..(abs x1)]
   let potentialYvs = [(abs y1)..(abs y0)]
   let potentialVs = Seq.allPairs potentialXvs potentialYvs
   launch, potentialVs
    
let day17 x0 x1 y0 y1 () =
    // target area: x=201..230, y=-99..-65
    // target area: x=20..30, y=-10..-5
    let launch, potentialVs = getParameters x0 y0 x1 y1 

    let launchProbe s =
        s |> Seq.map (launch >> isHit) |> Seq.choose id |> Seq.max |> int64
    launchProbe potentialVs
   
let day17part2 x0 x1 y0 y1 () =
    let launch, potentialVs = getParameters x0 y0 x1 y1
    // potentialVs |> Seq.map launch |> Seq.choose id |> Seq.length |> int64
    0L