(* Work in progress for next release! *)
(*
type t = int * int
and 'a u =
  | A of t list
  | B of { left : t; right : 'a }
  | C of 'a u
and v =
    { x : t; y : v u }
      [@@deriving traverse]
*)
(*
module Accu = struct
  type t = int list ref
end

let () =
  let module Iter = [%traverse: 'a u list]
    (Traverse.Applicative.Env (Accu) (Traverse.Applicative.Iter))
    (Traverse.Arity.A1) in
  let accu = ref [] in
  Iter.traverse (fun i accu -> Metapp.mutate (List.cons i) accu) [
    B { left = (0, 0); right = 1 };
    C (B { left = (0, 0); right = 2 })] accu;
  assert (List.rev !accu = [1; 2])

module Iter =
  Traverse_v.Make
   (Traverse.Applicative.Env (Accu) (Traverse.Applicative.Iter))
   (Traverse.Arity.A1)

class ['self] test_iter = object (self : 'self)
  inherit [_] Iter.traverse

  method! visit_int i accu =
    Metapp.mutate (List.cons i) accu
end

let v =
  { x = (1, 2);
    y = C (B { left = (3, 4); right = { x = (5, 6); y = A [(7, 8); (9, 10)]}})}

let () =
  let accu = ref [] in
  let iter = new test_iter in
  iter#visit_v v accu;
  assert (List.rev !accu = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])

module Map2 =
  Traverse_u.Make (Traverse.Applicative.Map) (Traverse.Arity.A2)

class ['self] test_map2 = object (self : 'self)
  inherit [_] Map2.traverse

  method! visit_int i j =
    i + j
end

let u1 = B { left = (1, 2); right = 3 }

let u2 = B { left = (4, 5); right = 6 }

let () =
  let map2 = new test_map2 in
  let u = map2#visit_u map2#visit_int u1 u2 in
  assert (u = B { left = (5, 7); right = 9 })
*)
