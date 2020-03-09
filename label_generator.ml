(* function which return a new label each call *)
let new_label =
  (* create a label generator *)
  let get_generator () =
    (* this is a trick to hide the reference generator updates *)
    let r = ref [] in
    (* increment a list of characters *)
    let rec inc l =
      match l with
      | []      -> ['a']
      | 'z'::xs -> 'a' :: (inc xs)
      | x::xs   -> char_of_int ( (int_of_char x) + 1 ) :: xs
    in
    (* each call of generator will return a new label in the following sequence
       a, b, ..., z, aa, ab, ..., az, ba, bb, ..., zz, aaa, ...
    *)
    let generator () =
      r := inc (!r);
      String.concat "" ( List.map (String.make 1) (List.rev (!r)) )
    in
    generator
  in
  get_generator();;

(* print first 1001 labels to test output of new_label() *)
(* for i = 0 to 1000 do Printf.printf "%s\n" ( new_label() ) done;; *)
