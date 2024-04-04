type term =
  | Var of string
  | Func of string * term list

let rec antiunify t1 t2 =
  match (t1, t2) with
  | (Var x, Var y) when x = y -> Var x
  | (Var x, _) -> Var x
  | (_, Var y) -> Var y
  | (Func(f1, ts1), Func(f2, ts2)) when f1 = f2 -> Func(f1, List.map2 antiunify ts1 ts2)
  | (Func(_, _), Func(_, _)) -> Var "X"

let rec unify t1 t2 subst =
  match t1, t2 with
  | Var x, _ ->
      if occurs_check x t2 then
        None
      else
        Some (extend_subst x t2 subst)
  | _, Var y ->
      if occurs_check y t1 then
        None
      else
        Some (extend_subst y t1 subst)
  | Func (f1, a1), Func (f2, a2) ->
      unify_terms_list a1 a2 subst

and unify_terms_list l1 l2 subst =
  match l1, l2 with
  | [], [] ->
      Some subst
  | t1 :: tl1, t2 :: tl2 ->
      (match unify t1 t2 subst with
      | Some subst' -> unify_terms_list tl1 tl2 subst'
      | None -> None)
  | _ ->
      None

and occurs_check x t =
  match t with
  | Var y -> x = y
  | Func (_, tl) -> List.exists (occurs_check x) tl

and extend_subst x t subst =
  (x, t) :: List.map (fun (y, t') -> (y, apply_subst [(x, t)] t')) subst

and apply_subst subst t =
  match t with
  | Var x ->
      (try List.assoc x subst with Not_found -> Var x)
  | Func (f, tl) ->
      Func (f, List.map (apply_subst subst) tl)

let unify_terms t1 t2 =
  unify t1 t2 []

let rec string_of_term = function
  | Var x -> x
  | Func (f, args) ->
      let args_str = List.map string_of_term args |> String.concat ", " in
      f ^ "(" ^ args_str ^ ")"


(*EXEMPLES*)

let example1 = Func("",[])
let example2 = Func("",[])
let example3 = Func ("f", [Var "x"; Func ("g", [Var "x"])])
let example4 = Func ("f", [Var "x"; Var "x"])

let test_antiunify t1 t2 =
  Printf.printf "got %s\n" (string_of_term (antiunify t1 t2))

let test_unification t1 t2 =
  match unify_terms t1 t2 with
  | Some subst ->
      print_endline "Unification successful!";
      List.iter (fun (x, t) -> Printf.printf "%s -> %s\n" x (string_of_term t)) subst
  | None ->
      print_endline "Unification failed!"

let () =
  test_unification example1 example2;
  test_unification example2 example3;
  test_unification example3 example4;
  test_antiunify example1 example2;
