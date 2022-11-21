module Vec2 = Pvec.Make (struct
  let branching_factor_log2 = 1
end)

module Vec4 = Pvec.Make (struct
  let branching_factor_log2 = 2
end)

module Vec32 = Pvec.Make (struct
  let branching_factor_log2 = 5
end)

(******************************************************************************
 * Two ad-hoc module tests that guided me during the first hours.
 *****************************************************************************)
let%test_module "Vec2" =
  (module struct
    open Vec2

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
      | false -> Format.ksprintf failwith "check '%s' failed" label
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
    open Vec32

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
      | false -> Format.ksprintf failwith "check '%s' failed" label
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

    let () = test 31
    let () = test 32
    let () = test 33
    let () = test 64
    let () = test 65
  end)
;;

(******************************************************************************
 * In-depth test with random actions. Compare vector implementations
 * against list-based specification. Test persistence by maintaining and using
 * multiple references to each trie.
 *****************************************************************************)

module Spec : Pvec.T = struct
  include List

  let empty () = []
  let append a t = t @ [ a ]
  let get i t = if i < 0 then None else List.nth_opt t i

  let set i v t =
    let n = List.length t in
    if i < 0 || i > n
    then None
    else if i = n
    then Some (append v t)
    else Some (List.mapi (fun j x -> if i = j then v else x) t)
  ;;

  let peek t = List.fold_left (fun _ el -> Some el) None t

  let pop t =
    match List.rev t with
    | [] -> None
    | hd :: tl -> Some (hd, List.rev tl)
  ;;

  let get_exn i t = List.nth t i

  let set_exn i x t =
    match set i x t with
    | None -> raise (Invalid_argument "out of bounds")
    | Some x -> x
  ;;

  let rev_to_seq t = List.to_seq (List.rev t)
  let to_list t = t
  let of_list l = l
  let rev_iter f t = List.rev t |> List.iter f
  let debug_pp _ _ = failwith "not implemented"
end

type 'el vectors_with_implementation =
  | V :
      { arr : 'vec array
      ; peek : 'vec -> 'el option
      ; pop : 'vec -> ('el * 'vec) option
      ; get : int -> 'vec -> 'el option
      ; set : int -> 'el -> 'vec -> 'vec option
      ; append : 'el -> 'vec -> 'vec
      ; to_list : 'vec -> 'el list
      ; length : 'vec -> int
      }
      -> 'el vectors_with_implementation

let vectors_with_implementation ~slots (module M : Pvec.T) =
  let open M in
  V { arr = Array.make slots (empty ()); pop; peek; get; set; append; to_list; length }
;;

type action =
  | Peek
  | Pop
  | Get of int
  | Set of int * int
  | Append of int

let action_to_string = function
  | Peek -> "Peek"
  | Pop -> "Pop"
  | Get i -> "Get " ^ string_of_int i
  | Set (i, _) -> "Set " ^ string_of_int i
  | Append _ -> "Append"
;;

let random_action length =
  match Random.int 5 with
  | 0 -> Peek
  | 1 -> Pop
  | 2 -> Get (Random.int (length + 1) - 1)
  | 3 -> Set (Random.int (length + 2) - 1, Random.int 256)
  | 4 -> Append (Random.int 256)
  | _ -> assert false
;;

let apply ?(fail_fast = false) (V a, V b) ~src ~dst = function
  | Peek -> a.peek a.arr.(src) = b.peek b.arr.(src)
  | Pop ->
    (match a.pop a.arr.(src), b.pop b.arr.(src) with
     | Some (x, a'), Some (y, b') ->
       a.arr.(dst) <- a';
       b.arr.(dst) <- b';
       x = y
     | None, None -> true
     | _ -> false)
  | Get i ->
    (match a.get i a.arr.(src), b.get i b.arr.(src) with
     | Some x, Some y -> x = y
     | None, None -> true
     | _ -> false)
  | Set (i, v) ->
    (match a.set i v a.arr.(src), b.set i v b.arr.(src) with
     | Some a', Some b' ->
       if fail_fast && a.to_list a' <> b.to_list b'
       then false
       else (
         a.arr.(dst) <- a';
         b.arr.(dst) <- b';
         true)
     | None, None -> true
     | _ -> false)
  | Append v ->
    let a' = a.append v a.arr.(src)
    and b' = b.append v b.arr.(src) in
    if fail_fast && a.to_list a' <> b.to_list b'
    then false
    else (
      a.arr.(dst) <- a';
      b.arr.(dst) <- b';
      true)
;;

let random ~slots a b n =
  let () = Random.init n in
  let (V a) = vectors_with_implementation ~slots a in
  let (V b) = vectors_with_implementation ~slots b in
  let state = V a, V b in
  let slot () = Random.int (Array.length a.arr) in
  for i = 1 to n / 2 do
    (* grow vectors randomly *)
    let src, dst = slot (), slot () in
    assert (apply state ~src ~dst (Append i))
  done;
  for _ = n / 2 to n do
    (* random steps *)
    let src, dst = slot (), slot () in
    let action = random_action (a.length a.arr.(src)) in
    if not (apply (V a, V b) ~src ~dst action) then failwith (action_to_string action)
  done;
  for i = 0 to slots - 1 do
    assert (a.to_list a.arr.(i) = b.to_list b.arr.(i))
  done
;;

let spec = (module Spec : Pvec.T)
let vec2 = (module Vec2 : Pvec.T)
let vec4 = (module Vec4 : Pvec.T)
let vec32 = (module Vec32 : Pvec.T)

let%test_unit "random vec2 10" = random ~slots:1 vec2 spec 10
let%test_unit "random vec2 100" = random ~slots:2 vec2 spec 100
let%test_unit "random vec2 1000" = random ~slots:4 vec2 spec 1000
let%test_unit "random vec2 10000" = random ~slots:8 vec2 spec 10000
let%test_unit "random vec4 10" = random ~slots:1 vec4 spec 10
let%test_unit "random vec4 100" = random ~slots:2 vec4 spec 100
let%test_unit "random vec4 1000" = random ~slots:4 vec4 spec 1000
let%test_unit "random vec4 10000" = random ~slots:8 vec4 spec 10000
let%test_unit "random vec32 10" = random ~slots:1 vec32 spec 10
let%test_unit "random vec32 100" = random ~slots:2 vec32 spec 100
let%test_unit "random vec32 1000" = random ~slots:4 vec32 spec 1000
let%test_unit "random vec32 10000" = random ~slots:8 vec32 spec 10000
let%test_unit "random vec32 100000" = random ~slots:8 vec32 vec4 100000
let%test_unit "random vec32 1000000" = random ~slots:4 vec32 vec4 1000000
let%test_unit "random vec32 1000000" = random ~slots:16 vec32 vec4 1000000
let%test_unit "random vec32 1000000" = random ~slots:1024 vec32 vec4 1000000

(******************************************************************************
 * of_sequence was quite a challenge!
 *****************************************************************************)

let of_sequence (module M : Pvec.T) n =
  let open M in
  let vec = init n Fun.id in
  for i = 0 to n - 1 do
    if not (get i vec = Some i)
    then (
      Printf.eprintf "# of_seq\n";
      debug_pp Format.err_formatter vec;
      Printf.eprintf "# append\n";
      debug_pp
        Format.err_formatter
        (Seq.init n Fun.id |> Seq.fold_left (fun v x -> append x v) (empty ()));
      Printf.eprintf "# of_seq; n - 1\n";
      debug_pp Format.err_formatter (init (n - 1) Fun.id);
      Printf.kprintf failwith "get %i/%i" i n)
  done;
  assert (get n vec = None)
;;

let%test_unit "of_sequence vec2 0" = of_sequence vec2 0
let%test_unit "of_sequence vec2 1" = of_sequence vec2 1
let%test_unit "of_sequence vec2 2" = of_sequence vec2 2
let%test_unit "of_sequence vec2 3" = of_sequence vec2 3

let%test_unit "of_sequence vec2 0..99" =
  for i = 0 to 99 do
    of_sequence vec2 i
  done
;;

let%test_unit "of_sequence vec4 0..99" =
  for i = 0 to 99 do
    of_sequence vec4 i
  done
;;

let%test_unit "of_sequence vec2 0..99" =
  for i = 0 to 99 do
    of_sequence vec2 i
  done
;;

let%test_unit "of_sequence vec32 1000..1099" =
  for i = 1000 to 1099 do
    of_sequence vec32 i
  done
;;
