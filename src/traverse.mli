type ('a, 'b) eq = Eq : ('a, 'a) eq
(** Equality witness between types. *)

(** Just a type `t`. Used for
- the type of the environment in {!module:Applicative.Env},
- the type of the accumulator in {!module:Applicative.Fold},
- the error type in {!module:Applicative.Result}. *)
module type TypeS = sig
  type t
end

(** {!modtype:Monoid.S} is a signature for a monoid
    (a type `t` with `zero` and `+`),
    and ['a] {!type:Monoid.t} is the type for monoids represented as
    first-class modules. *)
module Monoid : sig
  module type S = sig
    type t

    val zero : t

    val ( + ) : t -> t -> t
  end

  type 'a t = (module S with type t = 'a)
end

(** {!modtype:Functor.S} is a signature for a functor
    (a type ['a] `t` with `map`). *)
module Functor : sig
  module type S = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end
end

(** {!modtype:Applicative.S} is a signature for an applicative functor
    (a type ['a] `t` with `map`, `pure` and `apply`).
    [('a, 'a_t, 'b, 'b_t) t] {!type:Applicative.t} is a
    first-class representation for applicative functors, with a
    constructor `Make` to convert an applicative functor to its
    first-class representation.
    This module contains many instances of applicative functors. *)
module Applicative : sig
  module type S = sig
    include Functor.S

    val pure : 'a -> 'a t

    val apply : ('a -> 'b) t -> (unit -> 'a t) -> 'b t
    (** The second argument is delayed for its evaluation to be skipped
        if not necessary: it allows short-circuits with {!val:forall},
        {!val:exists}, etc. *)
  end

  module type MonomorphicS = sig
    module Applicative : S

    type a

    type a_t

    type b

    type b_t

    val eq_a : (a_t, a Applicative.t) eq

    val eq_b : (b_t, b Applicative.t) eq
  end

  type ('a, 'a_t, 'b, 'b_t) t = unit ->
      (module MonomorphicS with type a = 'a and type a_t = 'a_t
      and type b = 'b and type b_t = 'b_t)

  module type InstanceS = sig
    module Applicative : S

    val instance : ('a, 'a Applicative.t, 'b, 'b Applicative.t) t
  end

  module Make (Applicative : S) : InstanceS
  with module Applicative = Applicative

  module Iter : S with type 'a t = unit

  val iter : ('a, 'a Iter.t, 'b, 'b Iter.t) t

  module Map : S with type 'a t = 'a

  val map : ('a, 'a Map.t, 'b, 'b Map.t) t

  module Reduce (Monoid : Monoid.S) : S with type 'a t = Monoid.t

  val reduce : 'm Monoid.t -> ('a, 'm, 'b, 'm) t

  module Env (E : TypeS) (Base : S) : S with type 'a t = E.t -> 'a Base.t

  val env : ('a, 'b, 'c, 'd) t -> ('a, 'e -> 'b, 'c, 'e -> 'd) t

  module Fold (Accu : TypeS) : S with type 'a t = Accu.t -> Accu.t

  val fold : ('a, 'acc -> 'acc, 'c, 'acc -> 'acc) t

  module Pair (U : S) (V : S) : S with type 'a t = 'a U.t * 'a V.t

  val pair : ('a, 'b0, 'c, 'd0) t -> ('a, 'b1, 'c, 'd1) t ->
    ('a, 'b0 * 'b1, 'c, 'd0 * 'd1) t

  module Forall : S with type 'a t = bool

  val forall : ('a, bool, 'c, bool) t

  module Exists : S with type 'a t = bool

  val exists : ('a, bool, 'c, bool) t

  module Option (Base : S) : S with type 'a t = 'a Base.t option

  val option : ('a, 'b, 'c, 'd) t -> ('a, 'b option, 'c, 'd option) t

  module Result (Base : S) (Err : TypeS) : S
  with type 'a t = ('a Base.t, Err.t) result

  val result : ('a, 'b, 'c, 'd) t ->
    ('a, ('b, 'e) result, 'c, ('d, 'e) result) t
end

(** Traversal for lists. *)
module List (Applicative : Applicative.S) : sig
  val traverse : ('a -> 'b Applicative.t) -> 'a list -> 'b list Applicative.t
end

val list : ('a, 'b, 'a list, 'd) Applicative.t -> ('c -> 'b) -> 'c list -> 'd

(** Traversal for sequences. *)
module Seq (Applicative : Applicative.S) : sig
  val traverse : ('a -> 'b Applicative.t) -> 'a Seq.t -> 'b Seq.t Applicative.t
end

val seq :
    ('a, 'b, 'a Stdlib.Seq.t, 'd) Applicative.t -> ('c -> 'b) ->
      'c Stdlib.Seq.t -> 'd
