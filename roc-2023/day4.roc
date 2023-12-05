app "day4"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day4-input.txt" as puzzle : Str,
        "day4-example.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p2 puzzle))" |> Task.await
    Task.ok {}

p1 : Str -> Nat
p1 = \input ->
    scores = input
        |> Str.trimEnd
        |> Str.split "\n"
        |> List.map parseGame
        |> List.map scoreGame

    List.sum scores

p2 : Str -> Nat
p2 = \input ->
        games = input
            |> Str.trimEnd
            |> Str.split "\n"
            |> List.map parseGame
        l = List.len games

        initialCounts = List.walk (List.range { start: At 0, end: Before l }) (List.withCapacity l) \acc, _idx ->
            List.append acc 1

        # dbg initialCounts

        updatedCounts = List.walkWithIndex games initialCounts \gameCounts, game, idx ->
            c = scoreGame2 game
            # dbg gameCounts
            incremental = List.get gameCounts idx |> orCrash "missing count idx"

            List.walk (List.range { start: At (idx + 1), end: Before (idx + 1 + c) }) gameCounts \acc2, accIdx ->
                when List.get acc2 accIdx is
                    Ok current -> List.set acc2 accIdx (current + incremental)
                    Err _ -> acc2

        List.sum updatedCounts


Game : { numbers: List Nat, winningNumbers: List Nat }

parseGame: Str -> Game
parseGame = \line ->
    gamePart = Str.splitFirst line ": " |> orCrash "bad game" |> .after |> Str.splitFirst " | " |> orCrash "bad game 2"

    {
        numbers: (Str.split gamePart.after " " |> List.keepOks Str.toNat),
        winningNumbers: (Str.split gamePart.before " " |> List.keepOks Str.toNat),
    }

scoreGame2: Game -> Nat
scoreGame2 = \game ->
    game.numbers
        |> List.keepIf \n -> List.contains game.winningNumbers n
        |> List.len

scoreGame: Game -> Nat
scoreGame = \game ->
    c = game.numbers
        |> List.keepIf \n -> List.contains game.winningNumbers n
        |> List.len

    if c == 0 then 0
    else Num.powInt 2 (c - 1)

orCrash = \result, msg ->
    when result is
        Ok v -> v
        Err _e -> crash msg
