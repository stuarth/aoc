app "day1"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day1-input.txt" as puzzle : Str,
        "day1-example.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p1 puzzle))" |> Task.await
    _ <- Stdout.line "p2: \(Num.toStr (p2 puzzle))" |> Task.await
    Task.ok {}

isDigit = \s ->
    List.contains ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] s

calibrationValue = \line ->
    graphemes = Str.graphemes line

    first = List.findFirst graphemes isDigit |> Result.withDefault "0"
    last = List.findLast graphemes isDigit |> Result.withDefault "0"

    Str.toNat "\(first)\(last)"

p1 = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks calibrationValue
    |> List.sum

expect calibrationValue "1abc2" == Ok 12
expect calibrationValue "pqr3stu8vwx" == Ok 38
expect calibrationValue "a1b2c3d4e5f" == Ok 15
expect calibrationValue "treb7uchet" == Ok 77

p2 = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks calibrationValue2
    |> List.sum

digits = Dict.empty {}
    |> Dict.insert "1" "1"
    |> Dict.insert "2" "2"
    |> Dict.insert "3" "3"
    |> Dict.insert "4" "4"
    |> Dict.insert "5" "5"
    |> Dict.insert "6" "6"
    |> Dict.insert "7" "7"
    |> Dict.insert "8" "8"
    |> Dict.insert "9" "9"
    |> Dict.insert "one" "1"
    |> Dict.insert "two" "2"
    |> Dict.insert "three" "3"
    |> Dict.insert "four" "4"
    |> Dict.insert "five" "5"
    |> Dict.insert "six" "6"
    |> Dict.insert "seven" "7"
    |> Dict.insert "eight" "8"
    |> Dict.insert "nine" "9"


calibrationValue2 = \line ->
    graphemes = Str.graphemes line

    first = List.walkUntil graphemes "" \acc, c ->
        s = Str.concat acc c

        digit = Dict.walkUntil digits "" \err, k, v ->
            if Str.endsWith s k then Break v
            else Continue err

        when digit is
            "" -> Continue s
            _ -> Break digit

    last = List.walkBackwardsUntil graphemes "" \acc, c ->
        s = Str.concat c acc

        digit = Dict.walkUntil digits "" \err, k, v ->
            if Str.startsWith s k then Break v
            else Continue err

        if digit == "" then Continue s
        else Break digit

    Str.toNat "\(first)\(last)"


expect calibrationValue2 "1abc2" == Ok 12
expect calibrationValue2 "pqr3stu8vwx" == Ok 38
expect calibrationValue2 "a1b2c3d4e5f" == Ok 15
expect calibrationValue2 "treb7uchet" == Ok 77
expect calibrationValue2 "two1nine" == Ok 29
expect calibrationValue2 "eightwothree" == Ok 83
expect calibrationValue2 "abcone2threexyz" == Ok 13
expect calibrationValue2 "xtwone3four" == Ok 24
expect calibrationValue2 "4nineeightseven2" == Ok 42
expect calibrationValue2 "zoneight234" == Ok 14
expect calibrationValue2 "7pqrstsixteen" == Ok 76
