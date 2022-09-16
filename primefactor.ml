let rec factorize_helper = fun (factor, number) -> (
  match factor >= number with
  | true -> number
  | _ -> (match number mod factor = 0 with 
    | true -> factorize_helper (factor, number / factor)
    | _ -> factorize_helper (factor + 1, number)
  )
) in 
let factorize = fun number ->
  factorize_helper (2, number) in 
let test = factorize 10 in 
let _ = print_int test in 
let _ print_string "\n" in 
let answer = factorize 600851475143 in 
print_int answer