(** Create a non empty list *)
type non_empty
type empty

type (_, _) t =
  | (::) : 'a * ('a, _) t -> ('a, non_empty) t
  | [] : (_, empty) t

(* Now we have a list which contains at least one element *)

let rec f = function
  | e1 :: e2 :: es -> e1 + f (e2 :: es)
  | e :: [] -> e

let singleton a = a :: []

let hd = function x :: _ -> x
let tl = function
  | _ :: [] -> None
  | _ :: x :: xs -> Some (x :: xs)

let rec rev: type x. ('a, x) t -> ('a, non_empty) t -> ('a, non_empty) t = fun acc -> function
  | e :: [] -> e :: acc
  | e :: e2 :: es -> rev (e :: acc) (e2 :: es)

let (@) a b = rev b (rev [] a)

let rev x = rev [] x

let of_list =
  let rec inner acc = function
    | List.[] -> rev acc
    | List.(x :: xs) -> inner (x :: acc) xs
  in
  function
  | List.[] -> None
  | List.(x :: xs) -> Some (inner (singleton x) xs)

let rec to_list: type a. ('t, a) t -> 't List.t = function
  | x :: xs -> List.cons x (to_list xs)
  | [] -> List.[]

let rec iter f = function
  | x :: [] -> f x
  | x1 :: x2 :: xs -> f x1; iter f (x2 :: xs)

let rec fold_left ~init ~f = function
  | x :: [] -> f init x
  | x1 :: x2 :: xs -> fold_left ~init:(f init x1) ~f (x2 :: xs)

let reduce ~f = function
  | x :: [] -> x
  | x1 :: x2 :: xs -> fold_left ~init:x1 ~f (x2 :: xs)

let _ =
  let x = 4 :: 5 :: 6 :: [] in
  let y = 1 :: 2 :: 3 :: [] in
  Printf.printf "f: %d\n" (f x);
  Printf.printf "y: %d\n" (f y);
  Printf.printf "append:";
  iter (Printf.printf "%d ") (x @ y);
  Printf.printf "\n";
  Printf.printf "sum: %d\n" (reduce ~f:(+) (x @ y));
  ()




module Peano = struct
  type zero
  type succ
  type 'a s  = succ * 'a

  (* Sum it here ??? *)
  type (_, _) p =
    | (::) : 'a * ('a, 'b) p -> ('a, 'b s) p
    | [] : (_, zero) p

  let hd: ('a, _ s) p -> 'a = function x :: _ -> x

  let tl: ('b, 'a s) p -> ('b, 'a) p = function
    | _ :: xs -> xs

  let rec f: type a. ('t, a) p -> 't = function
    | x :: xs -> x + f xs
    | [] -> 0

  let rec g: type a. ('t, a s) p -> 't = function
    | x :: [] -> x
    | x :: x2 :: xs -> x + g (x2 :: xs)

  let _ =
    Printf.printf "Peano\n";
    let x = 5 :: 7 :: 12 :: [] in
    Printf.printf "f: %d\n" (f x);
    Printf.printf "g: %d\n" (g x);
    ()


end
