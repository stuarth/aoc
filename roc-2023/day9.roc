app "day9"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day9-input.txt" as puzzle : Str,
        "day9-example.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p1 puzzle))" |> Task.await
    Task.ok {}

p1 : Str -> I64
p1 = \input ->
    rows = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \r -> (parseRow r |> List.reverse)

    dbg rows

    withNext = List.map rows extrapolate

    dbg withNext

    List.walk withNext 0 \acc, row -> acc + (List.last row |> orCrash "row last")

parseRow = \line ->
    line
        |> Str.split " "
        |> List.keepOks Str.toI64

extrapolate: List I64 -> List I64
extrapolate = \row ->
    allZeros = \r -> List.all r \n -> n == 0
    nextRow = \r ->
        List.map2 (List.dropLast r 1) (List.dropFirst r 1) \e1, e2 -> e2 - e1

    extrapolateToZero : List (List I64) -> List (List I64)
    extrapolateToZero = \rows ->
        next = rows |> List.last |> orCrash "nextRow" |> nextRow

        if allZeros next then rows
        else extrapolateToZero (List.append rows next)

    extrapolated = extrapolateToZero [row]

    dbg extrapolated

    rollUp = List.walkBackwards extrapolated [] \acc, r ->
        # dbg ("acc", acc)
        # dbg ("r", r)
        last = List.last acc |> Result.try List.last |> Result.withDefault 0
        # dbg ("last", last)
        List.append acc (List.append r (last + (List.last r |> orCrash "last r")))

    # dbg ("rollup", rollUp)

    List.last rollUp |> orCrash "rollUp last"


orCrash = \result, msg ->
    when result is
        Ok v -> v
        Err _e -> crash msg
