
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