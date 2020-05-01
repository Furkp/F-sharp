module Dictionary

type Dictionary =
    | Leaf of bool
    | Node of bool * Map<char, Dictionary>

let empty (s:string) = 
    Array.fold (fun acc c -> 
        match acc with
            | Leaf b -> Node(b, Map.empty.Add(c, Leaf false))
            | Node(b, m) -> Node(b, m.Add(c, Leaf false))
    ) (Node(false, Map.empty)) (s.ToCharArray())

let rec insert (s:string) =
    function
    | Leaf _ when s.Length = 0
        -> Leaf true
    | Node(_, m) when s.Length = 0
        -> Node(true, m)
    | Node(b, m) when m.ContainsKey (s.Chars 0)
        -> Node (b, m.Add(s.Chars 0, insert (s.Substring 1) (m.Item(s.Chars 0))))
    | Leaf b
        -> Node(b, Map.empty.Add(s.Chars 0, insert (s.Substring 1) (Leaf false)))
    | Node(b, m) 
        -> Node(b, m.Add(s.Chars 0, insert (s.Substring 1) (Leaf false)))

let rec lookup (s:string) =
    function
    | Leaf b when s.Length = 0 
        -> b
    | Leaf _ 
        -> false
    | Node(b, m) when s.Length = 0
        -> b
    | Node(_, m) when m.ContainsKey(s.Chars 0)
        -> lookup (s.Substring 1) (m.Item (s.Chars 0))
    | Node(_, _) 
        -> false
  
let rec contains (s:string) =
    function
    | Leaf b when s.Length = 0 
        -> Some (Leaf(b))
    | Leaf _ 
        -> None
    | Node(b, m) when s.Length = 0
        -> Some (Node(b, m))
    | Node(_, m) when m.ContainsKey(s.Chars 0)
        -> contains (s.Substring 1) (m.Item (s.Chars 0))
    | Node(_, _) 
        -> None

let isEnd = 
    function
    | Leaf b -> b
    | Node (b, _) -> b    