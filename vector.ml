module Make (P : sig
  val branching_factor_log2 : int
end) : sig
  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val len : 'a t -> int
  val empty : unit -> 'a t
  val append : 'a -> 'a t -> 'a t
  val get : int -> 'a t -> 'a option
  val set : int -> 'a -> 'a t -> 'a t option
  val peek : 'a t -> 'a option
  val pop : 'a t -> ('a * 'a t) option
  val get_exn : int -> 'a t -> 'a
  val set_exn : int -> 'a -> 'a t -> 'a t
end = struct
  let () = if P.branching_factor_log2 < 1 then failwith "invalid branching_factor_log2"
  let bits = P.branching_factor_log2
  let width = 1 lsl bits
  let mask = width - 1
  let pp_array el fmt arr = Ppx_show_runtime.pp_list el fmt (Array.to_list arr)

  type 'a trie =
    | Empty
    | Leave of 'a array
    | Node of 'a trie array
  [@@deriving show { with_path = false }]

  type 'a t =
    { trie : 'a trie
    ; shift : int
    ; tail : 'a option array
    ; last : int
    }
  [@@deriving show { with_path = false }]

  let len t = t.last + 1
  let empty () = { trie = Empty; shift = 0; tail = Array.make width None; last = -1 }
  let trie_size t = (t.last lsr bits) lsl bits
  let tail_size t = (t.last land mask) + 1

  let tail_full t =
    assert (tail_size t + trie_size t = len t);
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
  let pop_tail _ = failwith "TODO"

  let pop t =
    if t.last = -1
    then None
    else if t.last = 0
    then Some (t.tail.(0) |> Option.get, empty ())
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
end

module Vector2 = Make (struct
  let branching_factor_log2 = 1
end)

module Vector32 = Make (struct
  let branching_factor_log2 = 5
end)

let%expect_test "growth2" =
  let open Vector2 in
  Array.fold_left
    (fun vec i ->
      Format.printf "step %i:%a\n" i (pp Format.pp_print_int) vec;
      append i vec)
    (empty ())
    (Array.init 6 Fun.id)
  |> ignore;
  [%expect
    {|
    step 0:{ trie = Empty; shift = 0; tail = [None; None]; last = -1 }
    step 1:
    { trie = Empty; shift = 0; tail = [Some (0); None]; last = 0 }
    step 2:
    { trie = Empty; shift = 0; tail = [Some (0); Some (1)]; last = 1 }
    step 3:
    { trie = Leave ([0; 1]); shift = 1; tail = [Some (2); None]; last = 2 }
    step 4:
    { trie = Leave ([0; 1]); shift = 1; tail = [Some (2); Some (3)]; last = 3 }
    step 5:
    { trie = Node ([Leave ([0; 1]); Leave ([2; 3])]); shift = 2;
      tail = [Some (4); None]; last = 4 } |}]
;;

let%test_module "Vec2" =
  (module struct
    open Vector2

    let init len =
      let vec = ref (empty ()) in
      for i = 0 to len - 1 do
        vec := append i !vec;
        assert (peek !vec = Some i)
      done;
      for i = 0 to len - 1 do
        vec := set_exn i i !vec
      done;
      !vec
    ;;

    let mem i t = get i t = Some i
    let non_mem i t = get i t = None

    let check vec label prop =
      match prop vec with
      | true -> ()
      | false ->
        Format.eprintf "%a\n" (pp Format.pp_print_int) vec;
        Format.ksprintf failwith "check '%s' failed" label
      | exception exn ->
        Format.eprintf "%a\n%!" (pp Format.pp_print_int) vec;
        raise exn
    ;;

    let test len =
      let vec = init len in
      check vec "-1" (non_mem (-1));
      check vec "0" (mem 0);
      check vec "len - 1" (mem (len - 1));
      check vec "len" (non_mem len);
      Array.iter (fun i -> check vec "all" (mem i)) (Array.init len Fun.id);
      Array.fold_left
        (fun (len, vec) _ ->
          let i, vec = pop vec |> Option.get in
          assert (i = len - 1);
          len - 1, vec)
        (len, vec)
        (Array.init len Fun.id)
      |> ignore
    ;;

    let () = test 1
    let () = test 2
    let () = test 3
    let () = test 4
    let () = test 5
    let () = test 6
    let () = test 7
    let () = test 8
    let () = test 9
    let () = test 10
    let () = test 11
    let () = test 12
    let () = test 13
    let () = test 14
    let () = test 15
    let () = test 16
    let () = test 99
    let () = test 1024
    let () = test 1025
  end)
;;

let%test_module "Vec32" =
  (module struct
    open Vector32

    let init len =
      let vec = ref (empty ()) in
      for i = 0 to len - 1 do
        vec := append i !vec
      done;
      !vec
    ;;

    let mem i t = get i t = Some i
    let non_mem i t = get i t = None

    let check vec prop =
      match prop vec with
      | true -> ()
      | false ->
        Format.eprintf "%a\n" (pp Format.pp_print_int) vec;
        failwith "check failed"
      | exception exn ->
        Format.eprintf "%a\n" (pp Format.pp_print_int) vec;
        raise exn
    ;;

    let test len =
      let vec = init len in
      check vec (non_mem (-1));
      check vec (mem 0);
      check vec (mem (len - 1));
      check vec (non_mem len);
      Array.iter (fun i -> check vec (mem i)) (Array.init len Fun.id)
    ;;

    let () = test 31
    let () = test 32
    let () = test 33
    let () = test 42
    let () = test 64
    let () = test 65
  end)
;;
