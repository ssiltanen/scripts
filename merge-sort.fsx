let merge<'a when 'a :> System.IComparable> (a: 'a[]) (b: 'a[]) =
    let mutable ai = 0
    let mutable bi = 0
    Array.length a + Array.length b
    |> Array.zeroCreate<'a>
    |> Array.scan (fun _ _ ->
        match ai < Array.length a, bi < Array.length b with
        | true, true ->
            if a[ai].CompareTo(b[bi]) <= 0 then
                ai <- ai + 1
                a[ai - 1]
            else
                bi <- bi + 1
                b[bi - 1]
        | false, true ->
            bi <- bi + 1
            b[bi - 1]
        | true, false ->
            ai <- ai + 1
            a[ai - 1]
        ) (Array.head a)
    |> Array.tail

let mergeSort<'a when 'a :> System.IComparable> (arr: 'a[]) =
    let rec sort arr =
        arr
        |> Array.splitInto 2
        |> function
        | arr when Array.length arr < 2 -> arr |> Array.collect id
        | [|a; b|] -> merge (sort a) (sort b)
        
    sort arr

let data = seq {0..20_000_000} |> Array.ofSeq |> Array.randomShuffle

#time

mergeSort data
