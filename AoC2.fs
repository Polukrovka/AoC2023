module AoC.AoC2

open System
open System.Collections.Generic
open System.IO
open NUnit.Framework

let input = File.ReadAllLines "in2.txt"

type Cube = { Color: string; Amount: int }

type Game =
    { Id: int
      Rounds: Dictionary<int, Cube[]> }

let ParseRound (s: string) =
    let cubes =
        s.Split ", "
        |> Seq.map (fun draw -> draw.Split " ")
        |> Seq.map (fun splitDraw ->
            { Color = splitDraw[1]
              Amount = int splitDraw[0] })
        |> Seq.toArray

    cubes

let ParseGame (s: string) =
    let rounds = Dictionary<int, Cube[]>()
    let split = s.Split "; "

    for i in 0 .. (split.Length - 1) do
        rounds.Add(i, ParseRound(split[i]))

    rounds

let IsCubeValid (cube: Cube) =
    match cube.Color with
    | "red" -> cube.Amount < 13
    | "green" -> cube.Amount < 14
    | "blue" -> cube.Amount < 15

let IsRoundValid (cubes: Cube[]) = cubes |> Array.forall IsCubeValid

let IsGameValid (game: Game) =
    let roundsAsSeq =
        seq {
            for entry in game.Rounds do
                entry.Value
        }

    roundsAsSeq |> Seq.forall IsRoundValid

let games =
    input
    |> Seq.map (fun s -> s.Split(": "))
    |> Seq.map (fun splits ->
        { Id = Int32.Parse(splits[0].Split(" ")[1])
          Rounds = ParseGame splits[1] })

let mutable x = 0

for game in games do
    if IsGameValid game then
        x <- x + game.Id

Console.WriteLine x

let GetPower (game: Game) =
    let minimums = Dictionary<string, int>()

    for entry in game.Rounds do
        for cube in entry.Value do
            if minimums.ContainsKey(cube.Color) && minimums.Item(cube.Color) < cube.Amount then
                minimums[cube.Color] <- cube.Amount
            else if not (minimums.ContainsKey(cube.Color)) then
                minimums.Add(cube.Color, cube.Amount)

    let mutable x = 1

    for kv in minimums do
        x <- x * kv.Value

    x

Console.WriteLine(games |> Seq.map GetPower |> Seq.sum)

[<TestFixture>]
type TestClass() =
    [<Test>]
    member this.TestSingleDrawRound() =
        let expected = [| { Color = "green"; Amount = 1 } |]
        let actual = ParseRound("1 green")
        Assert.That(actual, Is.EquivalentTo(expected))

    [<Test>]
    member this.TestTwoDrawRound() =
        let expected = [| { Color = "blue"; Amount = 3 }; { Color = "red"; Amount = 4 } |]
        let actual = ParseRound("3 blue, 4 red")
        Assert.That(actual, Is.EquivalentTo(expected))

    [<Test>]
    member this.TestSingleRoundGame() =
        let expected = Dictionary<int, Cube[]>()
        expected.Add(0, [| { Color = "green"; Amount = 1 } |])
        let actual = ParseGame("1 green")
        Assert.That(actual, Is.EquivalentTo(expected))

    [<Test>]
    member this.TestTwoRoundGame() =
        let expected = Dictionary<int, Cube[]>()
        expected.Add(0, [| { Color = "green"; Amount = 1 } |])
        expected.Add(1, [| { Color = "blue"; Amount = 3 }; { Color = "red"; Amount = 4 } |])
        let actual = ParseGame("1 green; 3 blue, 4 red")
        Assert.That(actual, Is.EquivalentTo(expected))

    [<Test>]
    member this.TestIsCubeValid() =
        Assert.That(IsCubeValid({ Color = "red"; Amount = 12 }), Is.True)
        Assert.That(IsCubeValid({ Color = "red"; Amount = 13 }), Is.False)
        Assert.That(IsCubeValid({ Color = "green"; Amount = 13 }), Is.True)
        Assert.That(IsCubeValid({ Color = "green"; Amount = 14 }), Is.False)
        Assert.That(IsCubeValid({ Color = "blue"; Amount = 14 }), Is.True)
        Assert.That(IsCubeValid({ Color = "blue"; Amount = 15 }), Is.False)

    [<Test>]
    member this.TestIsRoundValid() =
        Assert.That(IsRoundValid([| { Color = "red"; Amount = 12 }; { Color = "green"; Amount = 13 } |]), Is.True)
        Assert.That(IsRoundValid([| { Color = "red"; Amount = 12 }; { Color = "blue"; Amount = 15 } |]), Is.False)

    [<Test>]
    member this.TestIsValidOneRoundGame() =
        let rounds = Dictionary<int, Cube[]>()
        rounds.Add(0, [| { Color = "green"; Amount = 1 } |])
        let g = { Id = 1; Rounds = rounds }
        Assert.That(IsGameValid g, Is.True)

    [<Test>]
    member this.TestIsInvalidOneRoundGame() =
        let rounds = Dictionary<int, Cube[]>()
        rounds.Add(0, [| { Color = "green"; Amount = 14 } |])
        let g = { Id = 1; Rounds = rounds }
        Assert.That(IsGameValid g, Is.False)

    [<Test>]
    member this.TestIsValidTwoRoundGame() =
        let rounds = Dictionary<int, Cube[]>()
        rounds.Add(0, [| { Color = "green"; Amount = 1 } |])
        rounds.Add(1, [| { Color = "green"; Amount = 1 } |])
        let g = { Id = 1; Rounds = rounds }
        Assert.That(IsGameValid g, Is.True)

    [<Test>]
    member this.TestIsInvalidTwoRoundGame() =
        let rounds = Dictionary<int, Cube[]>()
        rounds.Add(0, [| { Color = "green"; Amount = 14 } |])
        rounds.Add(1, [| { Color = "green"; Amount = 14 } |])
        let g = { Id = 1; Rounds = rounds }
        Assert.That(IsGameValid g, Is.False)
