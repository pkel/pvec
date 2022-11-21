(** Persistent vectors.
    Bitmapped trie + tail optimization.
    Branching factor 32. *)

module type T = sig
  (** Vector holding elements of type ['a]. *)
  type 'a t

  (** [length v] returns the length of vector [v]. *)
  val length : 'a t -> int

  (** [empty ()] returns the new length of vector of length 0. *)
  val empty : unit -> 'a t

  (** [append x v] appends [x] to the end of vector [v] and returns the updated
      vector. *)
  val append : 'a -> 'a t -> 'a t

  (** [get i v] reads the i-th element from vector [v].

      Returns [None] if index [i] is out of bounds. *)
  val get : int -> 'a t -> 'a option

  (** [set i x v] replaces the i-th element of vector [v] with [x] and returns
      the updated vector.
      If [ i = length v] the element [x] is appended to [v].

      Returns [None] if index [i] is out of bounds. *)
  val set : int -> 'a -> 'a t -> 'a t option

  (** [peek v] returns the last element of vector [v]
      or [None] if [v] is empty. *)
  val peek : 'a t -> 'a option

  (** [pop v] removes the last element of vector [v] and returns the removed
      element together with the updated vector.

      Returns [None] if [v] is empty. *)
  val pop : 'a t -> ('a * 'a t) option

  (** [get_exn] is similar to {!get} but raises [Not_found] instead of returning
      [None]. *)
  val get_exn : int -> 'a t -> 'a

  (** [set_exn] is similar to {!set} but raises [Invalid_argument _] instead of returning
      [None]. *)
  val set_exn : int -> 'a -> 'a t -> 'a t

  (** [to_seq v] iterates over vector [v] front-to-back. *)
  val to_seq : 'a t -> 'a Seq.t

  (** [rev_to_seq v] iterates over vector [v] back-to-front. *)
  val rev_to_seq : 'a t -> 'a Seq.t

  (** [of_seq s] stores the elements of sequence [s] in a vector. *)
  val of_seq : 'a Seq.t -> 'a t

  (** [to_list v] converts vector [v] to a list. *)
  val to_list : 'a t -> 'a list

  (** [of_list v] converts list [l] to a vector. *)
  val of_list : 'a list -> 'a t

  (** [init n f] returns a vector of length [n] holding elements [f(0)], [f(1)],
      ... . *)
  val init : int -> (int -> 'a) -> 'a t

  (** [iter f v] applies [f] to elements of [v] in order of the vector's index. *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** [iter f v] applies [f] to elements of [v] in reverse order. *)
  val rev_iter : ('a -> unit) -> 'a t -> unit

  val debug_pp : Format.formatter -> 'a t -> unit
end

include T

(** Create vectors with custom branching factor.

    {[
module V = Make (struct
  let branching_factor_log2 = n
end)
    ]}
    uses branching factor 2ⁿ.
    The default implementation {!Pvec} uses branching factor 32.
   *)
module Make (_ : sig
  val branching_factor_log2 : int
end) : T
