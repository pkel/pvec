(* construct long lists, convert to sequence, consume *)

let scenario1_list ~n =
  let open List in
  init n Fun.id |> iter ignore
;;

let scenario1_vector ~n =
  let open Vector in
  init n Fun.id |> iter ignore
;;

let scenario1_array ~n =
  let open Array in
  init n Fun.id |> iter ignore
;;

let stats ~list ~vector ~array label =
  let open Float.Array in
  let m = length list / 2 in
  sort compare list;
  sort compare vector;
  sort compare array;
  let ref = Float.Array.get list (m / 2) in
  let list = Float.Array.map (fun x -> x /. ref) list in
  let vector = Float.Array.map (fun x -> x /. ref) vector in
  let array = Float.Array.map (fun x -> x /. ref) array in
  let l = get list in
  let v = get vector in
  let a = get array in
  Printf.printf "%s\n" label;
  Printf.printf "  lst: %2.2f, %2.2f...%2.2f\n" (l (m / 2)) (l (m / 4)) (l (m * 3 / 4));
  Printf.printf "  arr: %2.2f, %2.2f...%2.2f\n" (a (m / 2)) (a (m / 4)) (a (m * 3 / 4));
  Printf.printf "  vec: %2.2f, %2.2f...%2.2f\n" (v (m / 2)) (v (m / 4)) (v (m * 3 / 4))
;;

let scenario1 ~m ~n =
  let list = Float.Array.make m nan
  and vector = Float.Array.make m nan
  and array = Float.Array.make m nan in
  for i = 0 to m - 1 do
    let c = Mtime_clock.counter () in
    scenario1_list ~n;
    Float.Array.set list i (Mtime_clock.count c |> Mtime.Span.to_ns);
    let c = Mtime_clock.counter () in
    scenario1_vector ~n;
    Float.Array.set vector i (Mtime_clock.count c |> Mtime.Span.to_ns);
    let c = Mtime_clock.counter () in
    scenario1_array ~n;
    Float.Array.set array i (Mtime_clock.count c |> Mtime.Span.to_ns)
  done;
  stats ~list ~vector ~array ("scenario1/" ^ string_of_int n)
;;

let () = scenario1 ~m:250 ~n:100
let () = scenario1 ~m:250 ~n:1000
let () = scenario1 ~m:250 ~n:10000
let () = scenario1 ~m:250 ~n:100000
(* let () = scenario1 ~m:250 ~n:1000000 *)
