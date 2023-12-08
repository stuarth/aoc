app "day8"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day8-input.txt" as puzzle : Str,
        "day8-example2.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p1 puzzle))" |> Task.await
    Task.ok {}

p1 : Str -> Nat
p1 = \input ->
    map = parseMap (Str.trim input)

    dbg map

    stepsTillEnd map

stepsTillEnd: Map -> Nat
stepsTillEnd = \{ steps, nodes } ->
    # dbg (steps, nodes)

    f = \current, count ->
        # dbg (current, count)
        if current == "ZZZ" then count
        else
            idx = count % (List.len steps)
            nextChoice = List.get steps idx |> orCrash "missing step \(Num.toStr idx)"
            nextNode = (Dict.get nodes current |> orCrash "missing node \(nextChoice)")
            f (if nextChoice == "L" then nextNode.0 else nextNode.1) (count + 1)

    f "AAA" 0

Map: { steps: List Str, nodes: Dict Str (Str, Str) }

parseMap: Str -> Map
parseMap = \input ->
    parts = Str.splitFirst input "\n\n" |> orCrash "parseMap parts"

    {
        steps: parts.before |> Str.trim |> Str.graphemes,
        nodes: parts.after
            |> Str.split "\n"
            |> List.map parseNode
            |> Dict.fromList
    }

parseNode = \line ->
    parts = Str.splitFirst line " = " |> orCrash "parseNode \(line)"
    choiceParts = parts.after |> Str.replaceFirst "(" "" |> Str.replaceFirst ")" "" |> Str.splitFirst ", " |> orCrash "choiceParts"

    (parts.before, (choiceParts.before, choiceParts.after))


orCrash = \result, msg ->
    when result is
        Ok v -> v
        Err _e -> crash msg
