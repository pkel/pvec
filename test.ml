open Vector

module Spec : Vec = struct
  type 'a t = 'a list

  let length = List.length
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

  let to_list t = t
end

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

let start = Vector2.empty (), Spec.empty ()

let apply (a, b) = function
  | Peek -> if Vector2.peek a = Spec.peek b then Some (a, b) else None
  | Pop ->
    (match Vector2.pop a, Spec.pop b with
     | Some (x, a), Some (y, b) when x = y -> Some (a, b)
     | None, None -> Some (a, b)
     | _ -> None)
  | Get i ->
    (match Vector2.get i a, Spec.get i b with
     | Some x, Some y when x = y -> Some (a, b)
     | None, None -> Some (a, b)
     | _ -> None)
  | Set (i, v) ->
    (match Vector2.set i v a, Spec.set i v b with
     | Some a, Some b when Vector2.to_list a = Spec.to_list b -> Some (a, b)
     | None, None -> Some (a, b)
     | _ -> None)
  | Append v ->
    (match Vector2.append v a, Spec.append v b with
     | a, b when Vector2.to_list a = Spec.to_list b -> Some (a, b)
     | _ -> None)
;;

let fuzz n =
  let () = Random.init n in
  let state = ref start in
  for i = 1 to n / 2 do
    state := apply !state (Append i) |> Option.get
  done;
  assert (Vector2.length (fst !state) = n / 2);
  for _ = n / 2 to n do
    let len = Vector2.length (fst !state) in
    let action = random_action len in
    match apply !state action with
    | Some x -> state := x
    | None ->
      print_string "Spec: ";
      List.map string_of_int (snd !state |> Spec.to_list)
      |> String.concat ","
      |> print_endline;
      print_string "Vec2: ";
      List.map string_of_int (fst !state |> Vector2.to_list)
      |> String.concat ","
      |> print_endline;
      action_to_string action |> print_endline;
      failwith "test failed"
  done
;;

let%test_unit "fuzz vec2 10" = fuzz 10
let%test_unit "fuzz vec2 100" = fuzz 100
let%test_unit "fuzz vec2 1000" = fuzz 1000
let%test_unit "fuzz vec2 10000" = fuzz 10000

let start = Vector2.empty (), Vector32.empty ()

let apply (a, b) = function
  | Peek -> if Vector2.peek a = Vector32.peek b then Some (a, b) else None
  | Pop ->
    (match Vector2.pop a, Vector32.pop b with
     | Some (x, a), Some (y, b) when x = y -> Some (a, b)
     | None, None -> Some (a, b)
     | _ -> None)
  | Get i ->
    (match Vector2.get i a, Vector32.get i b with
     | Some x, Some y when x = y -> Some (a, b)
     | None, None -> Some (a, b)
     | _ -> None)
  | Set (i, v) ->
    (match Vector2.set i v a, Vector32.set i v b with
     | Some a, Some b when Vector2.to_list a = Vector32.to_list b -> Some (a, b)
     | None, None -> Some (a, b)
     | _ -> None)
  | Append v ->
    (match Vector2.append v a, Vector32.append v b with
     | a, b when Vector2.to_list a = Vector32.to_list b -> Some (a, b)
     | _ -> None)
;;

let fuzz n =
  let () = Random.init n in
  let state = ref start in
  for i = 1 to n / 2 do
    state := apply !state (Append i) |> Option.get
  done;
  assert (Vector2.length (fst !state) = n / 2);
  for _ = n / 2 to n do
    let len = Vector2.length (fst !state) in
    let action = random_action len in
    match apply !state action with
    | Some x -> state := x
    | None ->
      print_string "Vector32: ";
      List.map string_of_int (snd !state |> Vector32.to_list)
      |> String.concat ","
      |> print_endline;
      print_string "Vec2: ";
      List.map string_of_int (fst !state |> Vector2.to_list)
      |> String.concat ","
      |> print_endline;
      action_to_string action |> print_endline;
      failwith "test failed"
  done
;;

let%test_unit "fuzz vec32 10" = fuzz 10
let%test_unit "fuzz vec32 100" = fuzz 100
let%test_unit "fuzz vec32 1000" = fuzz 1000
let%test_unit "fuzz vec32 10000" = fuzz 10000

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
    open Vector32

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
