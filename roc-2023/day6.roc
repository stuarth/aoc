app "day6"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day6-input.txt" as puzzle : Str,
        "day6-example.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p1 puzzle))" |> Task.await
    Task.ok {}

p1 : Str -> Nat
p1 = \input ->
    races = parseRaces2 input
    # dbg races

    List.walk races 1 \sum, race ->
        # dbg (sum, race)
        sum * List.len (winningTimes race)

Race : { time: Nat, distance: Nat }

# Time:      7  15   30
# Distance:  9  40  200
parseRaces: Str -> List Race
parseRaces = \input ->
    lines = Str.trim input |> Str.split "\n"
    times = List.get lines 0
        |> orCrash "times"
        |> Str.replaceFirst "Time:" ""
        |> Str.trim
        |> Str.split " "
        |> List.keepOks \s -> Str.trim s |> Str.toNat
    distances = List.get lines 1
        |> orCrash "distance"
        |> Str.replaceFirst "Distance:" ""
        |> Str.trim
        |> Str.split " "
        |> List.keepOks \s -> Str.trim s |> Str.toNat

    List.map2 times distances \time, distance -> { time, distance }

parseRaces2: Str -> List Race
parseRaces2 = \input ->
    lines = Str.trim input |> Str.split "\n"
    time = List.get lines 0
        |> orCrash "times"
        |> Str.replaceFirst "Time:" ""
        |> Str.replaceEach " " ""
        |> Str.toNat
        |> orCrash "time"
    distance = List.get lines 1
        |> orCrash "distance"
        |> Str.replaceFirst "Distance:" ""
        |> Str.replaceEach " " ""
        |> Str.toNat
        |> orCrash "distnace"

    dbg (time, distance)

    [{ time, distance }]

winningTimes : Race -> List Nat
winningTimes = \{ time, distance } ->
    List.walk (List.range { start: At 0, end: Before time}) [] \acc, t ->
        # dbg (time, t, distance)
        if (time - t) * t > distance then List.append acc t
        else acc


orCrash = \result, msg ->
    when result is
        Ok v -> v
        Err _e -> crash msg
