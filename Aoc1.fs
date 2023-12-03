module AoC.Aoc1


open System
open System.IO
let input = File.ReadAllLines "in1.txt"
let part1 = 
    input 
    |> Array.map (fun arr -> Seq.toArray arr |> Array.filter (fun n -> Char.IsNumber n))
    |> Array.map (fun a -> (Array.head a).ToString() + (Array.last a).ToString())
    |> Array.map int
    |> Array.sum
Console.WriteLine(part1)

let words = [|"one"; "1"; "two"; "2";"three"; "3";"four"; "4";"five"; "5";"six"; "6";"seven"; "7";"eight"; "8";"nine"; "9"|]
let indices =
    input
    |> Array.map (fun s -> words |> Array.map (fun w -> [|s.IndexOf(w); s.LastIndexOf(w)|]))
    |> Array.map (fun a -> a |> Array.collect (fun s -> s))
let maxIndices = indices |> Array.map Array.max 
let minIndices = indices |> Array.map (fun a -> a|> Array.filter (fun n -> n > -1 )) |> Array.map Array.min
let part2 =
    [|0 .. indices.Length - 1|] 
    |> Array.map (fun i -> (Array.findIndex (fun n -> n = minIndices[i]) indices[i] / 4 + 1) * 10 + Array.findIndex (fun n -> n = maxIndices[i]) indices[i] / 4 + 1 )
    |> Array.sum
Console.WriteLine(part2)