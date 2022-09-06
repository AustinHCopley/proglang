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
in
    print_list (reverse [1; 2; 3]);
    print_list (
        map ((fun x -> x + 1), [1;2;3])
    );