open Traverse

let () =
  assert (list Applicative.(option map)
    (fun x -> Some (x + 1)) [1; 2; 3] = Some [2; 3; 4])

let () =
  let flag = ref 0 in
  assert (list Applicative.(option map)
    (fun f -> if f () then Some () else None)
      [(fun () -> flag := 1; true);
        (fun () -> false);
        (fun () -> flag := 2; true)] = None);
  assert (!flag = 1)
