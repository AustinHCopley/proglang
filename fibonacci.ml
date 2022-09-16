let rec fib_helper = fun (last2, last1, max) -> (
  let next = last2 + last1 in fib_helper (last1, next, max) -> 
    match next < max in
    | true -> (match next mod 2 = 0 with true -> next | _ -> 0) + fib_helper (last1, next, max)
    | _ -> 0
) in
(* wrapper for fib_helper, that starts at the first Fib number*)
let fib = fun max -> 
  fib_helper (0, 1, max) in 
let test = fib 21 in
let _ = print_int test in 
let _ = print_string "\n" in
let answer = fib 4000000 in
print_int answer
print_string "\n"