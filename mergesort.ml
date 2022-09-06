let rec
print_list = fun l -> match l with
    | [] -> ()
    | a::more -> print_int a; print_char ' '; print_list more
and
    reverse = fun l -> match l with
        | [] -> ()
        | a::more -> (reverse more) @ [a]
and
    map = fun (f, l) -> match l with
        | [] -> []
        | a::more -> (f a)::(map (f, more))
and 
    len = fun l -> match l with
        | [] -> 0
        | a::more -> 1 + (len more)
and 
    split = fun (l, n) -> let
        length = len l
    and
        split_helper = fun (acc, rest, c) match c * 2 < length
            | true -> (
                    match rest with
                    | [] -> ([], [])
                    | a::more -> split_helper (acc @ [a], more, c+1)
                )
            | false -> (acc, rest)
    in
        split_helper([], l, 0)
and
    merge = fun (left, right) -> match (left, right) with
        | ([], _) -> right
        | (_, []) -> left
        | (l::ls, r::rs) -> match l > r with
            | true -> r::(merge (left, rs))
            | false -> l::(merge (ls, right))
and
    mergesort = fun l ->
        let (left, right) = split left
        in
        let sorted_l = mergesort left in
        let sorted_r = mergesort right in
        merge (sorted_l, sorted_r)
in
    print_list (reverse [1; 2; 3]);
    print_list (
        map ((fun x -> x + 1), [1;2;3])
    );