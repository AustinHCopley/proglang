
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree;;

let rec treemap = fun f -> (
  fun t -> ( match t with
    | Empty -> Empty
    | Node(l, data, r) -> Node(
      treemap f l,
      f d,
      treemap f r
    )

  )
) in
  let add1ToEverything = treemap(fun x -> x + 1) in
  let add2ToEverything = treemap(fun x -> x + 2) in
(
    Node(Node(Empty, 4, Empty), 5, Node(Empty, 9, Node(Empty, 10, Empty)))
)

reduce f [1; 2; 3; 4; 5] = f (f (f (f (1,2), 3), 4), 5)
let rec listreduce = fun f -> (
  fun x -> (match, x with
    | [y] -> y
    | a::b::more -> reduce f ((f (a,b))::more)
  )
) in 
reduce (fun (x,y) -> x+y) [1; 2; 3; 4]

let rec treereduce = fun f -> (
  fun start -> (
    fun t -> (match t with
      | Empty -> start
      | Node(l, data, r) -> f(reduce f start l, data, reduce f start r)
    )
  )
) in
reduce (fun (l, data, r) -> l + d + r) 1
  Node(Node(Empty, 4, Empty), 5, Node(Empty, 9, Node(Empty, 10, Empty)))

  let treemin = reduce (fun (l, data, r) -> match l < data with
    | true -> (match l < r with true -> 1 | _ -> r)
    | _ -> match( d < r with true -> d | _ -> r)
    ) 9999999999 in 
    treemin(Node(Node(Empty, 4, Empty), 5, Node(Empty, 9, Node(Empty, 10, Empty))))