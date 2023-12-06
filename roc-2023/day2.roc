app "day2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day2-input.txt" as puzzle : Str,
        "day2-example.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p2 puzzle))" |> Task.await
    Task.ok {}

p1 : Str -> Nat
p1 = \input ->
    input
        |> Str.split "\n"
        |> List.keepOks toGame
        |> List.keepIf isPossible
        |> List.map .id
        |> List.sum

toGame = \line ->
    dbg line

    when Str.split line ": " is
        [gameIdPart, turnsPart] ->
            gameId = Str.replaceFirst gameIdPart "Game " "" |> Str.toNat |> Result.withDefault 0

            turnsParts = Str.split turnsPart "; "
                |> List.map \round ->
                                Str.split round ", "
                                    |> List.keepOks \colorCount ->
                                        Str.splitFirst colorCount " " |> Result.map \{ before, after } ->
                                            (after, Str.toNat before |> Result.withDefault 0)


            Ok { id: gameId, turns: List.map turnsParts Dict.fromList }


        _ -> Err "invalid line"

isPossible = \{ turns } ->
    sums = totals { turns }

    red = Dict.get sums "red" |> Result.withDefault 0
    green = Dict.get sums "green" |> Result.withDefault 0
    blue = Dict.get sums "blue" |> Result.withDefault 0

    red <= 12 && green <= 13 && blue <= 14

totals = \{ turns } ->
    List.walk turns (Dict.empty {}) \acc, turn ->
        Dict.walk turn acc \turnAcc, color, count ->
            Dict.update turnAcc color \v ->
                when v is
                    Missing -> Present count
                    Present presentCount -> Present (Num.max presentCount count)

power = \game ->
    sums = totals game

    red = Dict.get sums "red" |> Result.withDefault 0
    green = Dict.get sums "green" |> Result.withDefault 0
    blue = Dict.get sums "blue" |> Result.withDefault 0

    red * green * blue

p2 : Str -> Nat
p2 = \input ->
    input
        |> Str.split "\n"
        |> List.keepOks toGame
        |> List.map power
        |> List.sum
