module type T = sig
  type 'a t

  val length : 'a t -> int
  val empty : unit -> 'a t
  val append : 'a -> 'a t -> 'a t
  val get : int -> 'a t -> 'a option
  val set : int -> 'a -> 'a t -> 'a t option
  val peek : 'a t -> 'a option
  val pop : 'a t -> ('a * 'a t) option
  val get_exn : int -> 'a t -> 'a
  val set_exn : int -> 'a -> 'a t -> 'a t
  val to_seq : 'a t -> 'a Seq.t
  val rev_to_seq : 'a t -> 'a Seq.t
  val to_list : 'a t -> 'a list
  val init : int -> (int -> 'a) -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val rev_iter : ('a -> unit) -> 'a t -> unit
end

module Custom (P : sig
  val branching_factor_log2 : int
end) : T = struct
  let () = if P.branching_factor_log2 < 1 then failwith "invalid branching_factor_log2"
  let bits = P.branching_factor_log2
  let width = 1 lsl bits
  let mask = width - 1

  type 'a trie =
    | Empty
    | Leave of 'a array
    | Node of 'a trie array

  type 'a t =
    { trie : 'a trie
    ; shift : int
    ; tail : 'a option array
    ; last : int
    }

  let length t = t.last + 1
  let empty () = { trie = Empty; shift = 0; tail = Array.make width None; last = -1 }
  let trie_size t = (t.last lsr bits) lsl bits
  let tail_size t = (t.last land mask) + 1

  let tail_full t =
    assert (tail_size t + trie_size t = length t);
    t.last >= 0 && tail_size t >= width
  ;;

  let update_c j v = Array.mapi (fun i x -> if i = j then v else x)
  and update_f j f = Array.mapi (fun i x -> if i = j then f x else x)

  let push_tail t =
    let tail =
      Leave
        (Array.map
           (function
            | None -> failwith "push_tail: can only push full tails"
            | Some x -> x)
           t.tail)
    in
    let rec path_to_tail shift =
      assert (shift >= 0);
      if shift = 0
      then tail
      else (
        let arr = Array.make width Empty in
        let () = arr.(0) <- path_to_tail (shift - bits) in
        Node arr)
    in
    let t = { t with tail = Array.make width None } in
    if t.trie = Empty
    then { t with trie = tail; shift = bits }
    else if t.last + (1 lsr bits) > 1 lsl t.shift
    then (
      (* root overflow, trie is full; create additional layer at root, set old trie as
         first child, put tail into second child *)
      let arr = Array.make width Empty in
      let () =
        arr.(0) <- t.trie;
        arr.(1) <- path_to_tail (t.shift - bits)
      in
      { t with trie = Node arr; shift = t.shift + bits })
    else (
      (* tail has a place in the existing trie *)
      let rec insert shift = function
        | Empty -> path_to_tail shift
        | Node arr ->
          let j = (t.last lsr shift) land mask in
          Node (Array.mapi (fun i x -> if i = j then insert (shift - bits) x else x) arr)
        | Leave _ -> assert false
      in
      { t with trie = insert (t.shift - bits) t.trie })
  ;;

  let append x t =
    let t = if tail_full t then push_tail t else t in
    let key = t.last + 1 in
    let tail_key = key land mask in
    let tail =
      (* reuse tail if possible *)
      match t.tail.(tail_key) with
      | None ->
        t.tail.(tail_key) <- Some x;
        t.tail
      | Some _ -> update_c tail_key (Some x) t.tail
    in
    { t with last = key; tail }
  ;;

  let peek t = if t.last = -1 then None else Some (t.tail.(t.last land mask) |> Option.get)

  let pop_tail t =
    (* last element of tail was popped; (t.last >>> bits) points to the rightmost leave of
       the trie; we now remove this leave and set it as tail. *)
    let rec extract shift = function
      | Empty -> assert false
      | Node arr ->
        let idx = (t.last lsr shift) land mask in
        (match extract (shift - bits) arr.(idx) with
         | Empty, tail when idx = 0 -> Empty, tail
         | x, tail -> Node (update_c idx x arr), tail)
      | Leave arr -> Empty, Array.map Option.some arr
    in
    let trie, tail, shift =
      match extract (t.shift - bits) t.trie with
      | Leave _, _ -> assert false
      | Node arr, tail when arr.(1) = Empty ->
        (* root node becomes redundant, shrink trie *)
        arr.(0), tail, t.shift - bits
      | (Node _ as trie), tail ->
        (* base case *)
        trie, tail, t.shift
      | Empty, tail ->
        (* we popped the last leave from the trie *)
        Empty, tail, 0
    in
    { t with trie; tail; shift }
  ;;

  let pop t =
    if t.last = -1
    then None
    else (
      let r = t.tail.(t.last land mask) |> Option.get in
      let t = { t with last = t.last - 1 } in
      let t = if tail_full t (* it's not full but empty *) then pop_tail t else t in
      Some (r, t))
  ;;

  let get key t =
    let rec find shift = function
      | Node arr -> find (shift - bits) arr.((key lsr shift) land mask)
      | Leave arr -> Some arr.(key land mask)
      | Empty -> assert false
    in
    if key < 0 || key > t.last
    then None
    else if key >= trie_size t
    then t.tail.(key land mask)
    else find (t.shift - bits) t.trie
  ;;

  let set key v t =
    let rec in_trie shift = function
      | Node arr ->
        Node (update_f ((key lsr shift) land mask) (in_trie (shift - bits)) arr)
      | Leave arr -> Leave (update_c (key land mask) v arr)
      | Empty -> assert false
    in
    if key < 0
    then None
    else if key = t.last + 1
    then Some (append v t)
    else if key > t.last
    then None
    else if key >= trie_size t
    then Some { t with tail = update_c (key land mask) (Some v) t.tail }
    else Some { t with trie = in_trie (t.shift - bits) t.trie }
  ;;

  let get_exn key t =
    match get key t with
    | None -> raise Not_found
    | Some x -> x
  ;;

  let set_exn key v t =
    match set key v t with
    | None -> raise (Invalid_argument "out of bounds")
    | Some x -> x
  ;;

  let to_seq t =
    let open Seq in
    let rec trie = function
      | Empty -> empty
      | Leave a -> Array.to_seq a
      | Node a -> Array.to_seq a |> Seq.concat_map trie
    in
    let tail = init (tail_size t) (fun i -> Array.get t.tail i |> Option.get) in
    append (trie t.trie) tail
  ;;

  let array_rev_to_seq arr =
    let open Seq in
    let n = Array.length arr in
    init n (fun i -> arr.(n - i - 1))
  ;;

  let rev_to_seq t =
    let open Seq in
    let rec trie = function
      | Empty -> empty
      | Leave a -> array_rev_to_seq a
      | Node a -> array_rev_to_seq a |> Seq.concat_map trie
    in
    let tail =
      let n = tail_size t in
      init n (fun i -> Array.get t.tail (n - i - 1) |> Option.get)
    in
    append tail (trie t.trie)
  ;;

  let to_list t = rev_to_seq t |> Seq.fold_left (fun acc el -> el :: acc) []

  let init n f =
    (* TODO. avoid allocating n-1 headers *)
    Seq.init n f |> Seq.fold_left (fun acc el -> append el acc) (empty ())
  ;;

  let iter f t = to_seq t |> Seq.iter f
  let rev_iter f t = rev_to_seq t |> Seq.iter f
end

include Custom (struct
  let branching_factor_log2 = 5
end)
