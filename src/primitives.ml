module Atomic (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let traverse reference_value =
    Arity.Pred.destruct Zero
      (fun value ->
        if reference_value = value then
          Unit
        else
          raise Modules.StructuralMismatch)
      (function Unit ->
        Applicative.pure reference_value)
end

module Lazy (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let traverse traverse_'a =
    Arity.destruct (Succ Zero)
      (fun (lazy value) -> Cons (value, Unit))
      (function Cons (fx, Unit) ->
        Applicative.map Lazy.from_val (fx traverse_'a))
end

module List (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let rec traverse traverse_'a l =
    match l with
    | [] ->
        Arity.Pred.destruct Zero
          (function
            | [] -> Unit
            | _ :: _ -> raise Modules.StructuralMismatch)
          (function Unit ->
            Applicative.pure [])
    | hd :: tl ->
        Arity.Pred.destruct (Succ (Succ Zero))
          (function
            | [] -> raise Modules.StructuralMismatch
            | hd :: tl -> Cons (hd, Cons (tl, Unit)))
          (function Cons (hd', Cons (tl', Unit)) ->
            Applicative.apply
              (Applicative.map List.cons (hd' (traverse_'a hd)))
              (fun () -> (tl' (traverse traverse_'a tl))))
end

module Array (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  module List = List (Applicative) (Arity)

  let rec traverse traverse_'a =
    Arity.destruct (Succ Zero)
      (fun a -> Cons (Array.to_list a, Unit))
      (function Cons (fx, Unit) ->
        Applicative.map Array.of_list (fx (List.traverse traverse_'a)))
end

module Option (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let rec traverse traverse_'a o =
    match o with
    | None ->
        Arity.Pred.destruct Zero
          (function
            | None -> Unit
            | Some _ -> raise Modules.StructuralMismatch)
          (function Unit ->
            Applicative.pure None)
    | Some x ->
        Arity.Pred.destruct (Succ Zero)
          (function
            | None -> raise Modules.StructuralMismatch
            | Some x -> Cons (x, Unit))
          (function Cons (fx, Unit) ->
            Applicative.map Option.some (fx (traverse_'a x)))
end

module Ref (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let rec traverse traverse_'a =
    Arity.destruct (Succ Zero)
      (fun x -> Cons (!x, Unit))
      (function Cons (fx, Unit) ->
        Applicative.map ref (fx traverse_'a))
end

module Result (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let rec traverse traverse_'a traverse_'b o =
    match o with
    | Ok x ->
        Arity.Pred.destruct (Succ Zero)
          (function
            | Ok x -> Cons (x, Unit)
            | Error _ -> raise Modules.StructuralMismatch)
          (function Cons (fx, Unit) ->
            Applicative.map Result.ok (fx traverse_'a))
    | Error x ->
        Arity.Pred.destruct (Succ Zero)
          (function
            | Ok _ -> raise Modules.StructuralMismatch
            | Error x -> Cons (x, Unit))
          (function Cons (fx, Unit) ->
            Applicative.map Result.error (fx traverse_'b))
end

module Seq (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  let rec traverse traverse_'a s =
    match s () with
    | Seq.Nil ->
        Arity.Pred.destruct Zero
          (function s ->
            match s () with
            | Seq.Nil -> Unit
            | Seq.Cons _ -> raise Modules.StructuralMismatch)
          (function Unit ->
            Applicative.pure Seq.empty)
    | Seq.Cons (hd, tl) ->
        Arity.Pred.destruct (Succ (Succ Zero))
          (function s ->
            match s () with
            | Seq.Nil -> raise Modules.StructuralMismatch
            | Seq.Cons (hd, tl) -> Cons (hd, Cons (tl, Unit)))
          (function Cons (hd', Cons (tl', Unit)) ->
            Applicative.apply
              (Applicative.map Seq.cons (hd' (traverse_'a hd)))
              (fun () -> (tl' (traverse traverse_'a tl))))
end

module Classes (Applicative: Modules.Applicative.S)
    (Arity : Modules.Arity.NonNullS) = struct
  class virtual ['self] traverse =
    object (self : 'self)
      method visit_array :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a Stdlib.Array.t, 'a Stdlib.Array.t Applicative.t) Arity.t =
        let module Traverse = Array (Applicative) (Arity) in
        Traverse.traverse

      method visit_bool : (bool, bool Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_bytes : (bytes, bytes Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_char : (char, char Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_float : (float, float Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_int : (int, int Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_int32 : (int32, int32 Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_int64 : (int64, int64 Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_lazy_t :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a Stdlib.Lazy.t, 'a Stdlib.Lazy.t Applicative.t) Arity.t =
        let module Traverse = Lazy (Applicative) (Arity) in
        Traverse.traverse

      method visit_list :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a list, 'a list Applicative.t) Arity.t = fun visit_'a l ->
        match l with
        | [] ->
            Arity.Pred.destruct Zero
              (function
                | [] -> Unit
                | _ :: _ -> raise Modules.StructuralMismatch)
              (function Unit ->
                Applicative.pure [])
        | hd :: tl ->
            Arity.Pred.destruct (Succ (Succ Zero))
              (function
                | [] -> raise Modules.StructuralMismatch
                | hd :: tl -> Cons (hd, Cons (tl, Unit)))
              (function Cons (hd', Cons (tl', Unit)) ->
                Applicative.apply
                  (Applicative.map Stdlib.List.cons (hd' (visit_'a hd)))
                  (fun () -> (tl' (self#visit_list visit_'a tl))))

      method visit_nativeint : (nativeint, nativeint Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_option :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a option, 'a option Applicative.t) Arity.t =
        let module Traverse = Option (Applicative) (Arity) in
        Traverse.traverse

      method visit_ref :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a option, 'a option Applicative.t) Arity.t =
        let module Traverse = Option (Applicative) (Arity) in
        Traverse.traverse

      method visit_result :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a option, 'a option Applicative.t) Arity.t =
        let module Traverse = Option (Applicative) (Arity) in
        Traverse.traverse

      method visit_seq :
        'a . ('a, 'a Applicative.t) Arity.t ->
          ('a Stdlib.Seq.t, 'a Stdlib.Seq.t Applicative.t) Arity.t =
      fun visit_'a s ->
        match s () with
        | Stdlib.Seq.Nil ->
            Arity.Pred.destruct Zero
              (function s ->
                match s () with
                | Stdlib.Seq.Nil -> Unit
                | Stdlib.Seq.Cons _ -> raise Modules.StructuralMismatch)
              (function Unit ->
                Applicative.pure Stdlib.Seq.empty)
        | Stdlib.Seq.Cons (hd, tl) ->
            Arity.Pred.destruct (Succ (Succ Zero))
              (function s ->
                match s () with
                | Stdlib.Seq.Nil -> raise Modules.StructuralMismatch
                | Stdlib.Seq.Cons (hd, tl) -> Cons (hd, Cons (tl, Unit)))
              (function Cons (hd', Cons (tl', Unit)) ->
                Applicative.apply
                  (Applicative.map Stdlib.Seq.cons (hd' (visit_'a hd)))
                  (fun () -> (tl' (self#visit_seq visit_'a tl))))

      method visit_string : (string, string Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse

      method visit_unit : (unit, unit Applicative.t) Arity.t =
        let module Traverse = Atomic (Applicative) (Arity) in
        Traverse.traverse
    end
end
