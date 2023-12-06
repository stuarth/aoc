app "day5"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "day5-input.txt" as puzzle : Str,
        "day5-example.txt" as example : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "p1: \(Num.toStr (p1 puzzle))" |> Task.await
    Task.ok {}

p1 : Str -> Nat
p1 = \input ->
    pieces = Str.split input "\n\n" |> List.map Str.trim

    seeds = parseSeeds2 (List.first pieces |> orCrash "seed part")
    filters = List.map (List.dropFirst pieces 1) parseFilter

    # dbg filters

    seedValues = List.map seeds \s -> applyFilters s filters

    List.min seedValues |> orCrash "no min"

applyFilters: Nat, List Filter -> Nat
applyFilters = \seed, filters ->
    applyFilter = \idx, filter ->
        # dbg ("idx", idx)
        range = List.findFirst filter \r -> r.source <= idx && (r.source + r.len) > idx

        # dbg (idx, range)
        (Result.map range \r -> idx + r.dest- r.source) |> Result.withDefault idx

    List.walk filters seed applyFilter

parseSeeds: Str -> List Nat
parseSeeds = \line ->
    seedStrs = Str.splitFirst line "seeds: " |> orCrash "parseSeeds" |> .after

    seedStrs |> Str.split " " |> List.keepOks Str.toNat

parseSeeds2: Str -> List Nat
parseSeeds2 = \line ->
    seedStrs = Str.splitFirst line "seeds: " |> orCrash "parseSeeds2" |> .after

    nums = seedStrs |> Str.split " " |> List.keepOks Str.toNat

    List.walk (List.chunksOf nums 2) [] \acc, chunk ->
        start = List.get chunk 0 |> orCrash "start"
        len = List.get chunk 1 |> orCrash "len"

        List.concat acc (List.range { start: At start, end: At (start + len) })

Filter: List { source: Nat, dest: Nat, len: Nat }

parseFilter: Str -> Filter
parseFilter = \lines ->
    toNums : Str -> List Nat
    toNums = \l -> Str.split l " " |> List.map (\s -> Str.toNat s |> orCrash "toNat")

    # the destination range start, the source range start, and the range length
    Str.split lines "\n"
        |> List.dropFirst 1
        |> List.map toNums
        |> List.map \l -> {
            dest: List.get l 0 |> orCrash "dest",
            source: List.get l 1 |> orCrash "source",
            len: List.get l 2 |> orCrash "len"
        }

orCrash = \result, msg ->
    when result is
        Ok v -> v
        Err _e -> crash msg
