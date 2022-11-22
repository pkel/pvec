(** Persistent vectors.
    Bitmapped trie + tail optimization.
    Branching factor 32. *)

(**/**) (* hide module T *)

module type T = sig
  (** Persistent vectors with elements of type ['a]. *)
  type 'a t

  (** [length v] returns the number of elements in vector [v]. *)
  val length : 'a t -> int

  (** [empty ()] returns a new vector of length 0. *)
  val empty : unit -> 'a t

  (** [init n f] returns a vector of length [n] holding the elements [f(0)], [f(1)], ... . *)
  val init : int -> (int -> 'a) -> 'a t

  (** [append x v] appends [x] to the end of vector [v] and returns the updated
      vector. *)
  val append : 'a -> 'a t -> 'a t

  (** [set i x v] replaces the i-th element of vector [v] with [x] and returns
      the updated vector.
      For [ i = length v], [set i x v] equals [append x v]. *)
  val set : int -> 'a -> 'a t -> 'a t option
  (** Returns [None] if index [i] is out of bounds. *)

  (** [get i v] reads the i-th element from vector [v]. *)
  val get : int -> 'a t -> 'a option
  (** Returns [None] if index [i] is out of bounds. *)

  (** [peek v] returns the last element of vector [v]
      or [None] if [v] is empty. *)
  val peek : 'a t -> 'a option

  (** [pop v] removes the last element of vector [v] and returns the removed
      element together with the updated vector. *)
  val pop : 'a t -> ('a * 'a t) option
  (** Returns [None] if [v] is empty. *)

  (** {1 Unsafe of get and set} *)

  (** [get_exn] is similar to {!get} but raises [Not_found] instead of returning
      [None]. *)
  val get_exn : int -> 'a t -> 'a

  (** [set_exn] is similar to {!set} but raises [Invalid_argument _] instead of returning
      [None]. *)
  val set_exn : int -> 'a -> 'a t -> 'a t

  (** {1 Iterators} *)

  (** Like {!List.map}. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Like {!List.mapi}. *)
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

  (** Like {!List.fold_left}. *)
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** Similar to {!List.fold_right} but does not allocate additional memory. *)
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Like {!List.iter}. *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** Like {!iter} but in reverse order. *)
  val rev_iter : ('a -> unit) -> 'a t -> unit

  (** {1 Converters} *)

  (** [to_seq v] iterates over vector [v] front-to-back. *)
  val to_seq : 'a t -> 'a Seq.t

  (** [rev_to_seq v] iterates over vector [v] back-to-front. *)
  val rev_to_seq : 'a t -> 'a Seq.t

  (** [of_seq s] stores the elements of sequence [s] in a vector. *)
  val of_seq : 'a Seq.t -> 'a t

  (** [to_list v] converts vector [v] to a list. *)
  val to_list : 'a t -> 'a list

  (** [rev_to_list v] converts vector [v] to a list; reverse order. *)
  val rev_to_list : 'a t -> 'a list

  (** [of_list v] converts list [l] to a vector. *)
  val of_list : 'a list -> 'a t

  (** [to_array v] converts vector [v] to an array. *)
  val to_array : 'a t -> 'a array

  (** [rev_to_array v] converts vector [v] to an array; reverse order. *)
  val rev_to_array : 'a t -> 'a array

  (** [of_array v] converts array [l] to a vector. *)
  val of_array : 'a array -> 'a t

  (**/**) (* hide debug functionality *)

  (** [debug_pp fmt v] prints the (otherwise opaque) data structure of [v] to [fmt]. *)
  val debug_pp : Format.formatter -> 'a t -> unit
end

(**/**) (* end hiding module T *)

include T (* odoc inlines the documentation of module T here *)

(** {1 Custom vectors}

    The default vector {!t} uses branching factor 32. You may create vectors
    with custom branching factors. After doing
    {[
module V = Pvec.Make (struct
  let branching_factor_log2 = n
end)
    ]}
    module [V] has the same interface as {!Pvec} but its vectors use branching
    factor 2ⁿ.
*)

module Make (_ : sig
  val branching_factor_log2 : int
end) : T
