namespace MultiSet
module MultiSet =

    [<StructuredFormatDisplay("{AsString}")>]
    type MultiSet<'a when 'a : comparison> = 
        | MultiSet of Map<'a, uint32>
        override this.ToString() =
            let fold f acc (MultiSet(s)) = Map.fold f acc s
            let str = fold (fun acc k v -> acc+"("+k.ToString()+", #"+v.ToString()+"), ") "{" this
            str.Substring(0, str.Length-2) + "}"
        member this.AsString = this.ToString()


    let empty = 
        MultiSet(Map.empty)

    let isEmpty (MultiSet(s)) = 
        s.IsEmpty

    let size (MultiSet(s)) = 
        Map.fold (fun acc _ v -> acc + v) 0u s

    let contains a (MultiSet(s)) = 
        s.ContainsKey a

    let numItems a (MultiSet(s)) = 
        match s.TryFind(a) with
        | Some x -> x
        | None -> 0u

    let add a n (MultiSet(s)) = 
        match s.TryFind(a) with
        | Some x -> MultiSet(s.Add (a, n+x))
        | None -> MultiSet(s.Add (a, n))

    let addSingle a s = 
        add a 1u s

    let remove a n (MultiSet(s)) =
        match s.TryFind(a) with
        | Some x when x <= n -> MultiSet (s.Remove(a))
        | Some x -> MultiSet (s.Add(a, x-n))
        | None -> MultiSet s

    let removeSingle a s =
        remove a 1u s

    let fold f acc (MultiSet(s)) = 
        Map.fold f acc s

    let foldBack f (MultiSet(s)) acc =
        Map.foldBack f s acc

    let map f s = 
        fold (fun acc key value -> add (f key) value acc ) empty s

    let ofList lst =
        List.fold (fun acc v -> addSingle v acc ) empty lst

    let toList s =
        let rec addList l a = 
            function
            | 0 -> l 
            | n -> addList (l@[a]) (a) (n-1)
        fold (fun acc k v -> addList acc k (int v)) [] s

    let union s1 s2 =
        fold (fun (MultiSet(acc)) key value -> 
            match acc.TryFind(key) with
                | Some x when x < value -> add key (value-x) (MultiSet(acc))
                | Some x -> MultiSet acc
                | None -> add key value (MultiSet(acc))
        ) s1 s2

    let sum s1 s2 =
        fold (fun acc key value -> add key value acc ) s1 s2

    let subtract s1 s2 =
        fold (fun acc key value -> remove key value acc ) s1 s2

    let banan = 1

    let intersection s1 s2 =
        subtract(union s1 s2) (union (subtract s1 s2) (subtract s2 s1))

    let permutations s = 
        List.fold (fun acc x -> (x, (removeSingle x s))::acc) [] (toList s)