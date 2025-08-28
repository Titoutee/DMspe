type direction = B | H | D | G

(* Définition de quelques vallées pour les tests*)
let vallee_1 = [B; B; B; D; D; B; D; B; B; B; D; D; B; D;
                D; D; H; D; H; D; H; H; H; D; H; D; D; H]
let vallee_2 = [B; B; B; B; D; D; H; D; D; H; H; H; D; H; H; H]
let vallee_3 = [B; B; B; D; B; B; B; D; D; B; D; D; H; H; H; H]
let vallee_4 = [B; B; B; D; D; B; D; D; D; D; B; B; B; B; B; B; 
                D; H; H; H; H; H; H; H; D; D; D; D; D; H; D; D]


(*** Question 1 ***)
let rec list_len l = match l with 
  | [] -> 0
  | h::t -> 1 + list_len t;;

let est_sans_rebroussement (g : direction list) : bool =
  (* Cas g = [_] *)
  if list_len g = 1 then begin
    let f::_ = g in if (f = B || f = H) then false else true end else
(* Cas autre: g contient str. plus qu'un élément *)
  let forbidden = ref H in
  let first = ref true in 
  let rec aux li = match li with
    | [] -> true
    | f::[] ->if (!first) then (f <> H) else (f <> B)
    | h::t -> let res = h <> !forbidden in forbidden := (match h with
      | B -> H
      | H -> B
      | D -> G
      | G -> D);
      first := false;
      res && (aux t) in 
  aux g;;

(*** Tests question 1 ***)
let _ =
  assert (not (est_sans_rebroussement [B; B; G; D; B; B]));
  assert (not (est_sans_rebroussement [B; B; D; G; B; B]));
  assert (not (est_sans_rebroussement [G; G; H; B; D; D]));
  assert (not (est_sans_rebroussement [G; G; B; H; D; D]));
  assert (not (est_sans_rebroussement [H; G; G; B; B; D]));
  assert (not (est_sans_rebroussement [G; G; B; B; D; B]));
  assert (not (est_sans_rebroussement [B]));
  assert (not (est_sans_rebroussement [H]));
  assert (est_sans_rebroussement [D]);
  assert (est_sans_rebroussement [G]);
  assert (est_sans_rebroussement vallee_1);
  assert (est_sans_rebroussement vallee_2);
  assert (est_sans_rebroussement vallee_3);
  assert (est_sans_rebroussement vallee_4);
  Printf.printf "\nTous les tests de la question 1 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n";;


  (*** Question 2 ***)
let est_une_vallee (g : direction list) : bool =
  let up = ref false in
  let rec aux li = match li with
    | [] -> true
    | h::t -> match h with
      | H -> up := true; true && aux t
      | B when !up -> false
      | B -> true && aux t
      | D -> true && aux t
      | G -> false
in aux g && est_sans_rebroussement g;;

(*** Tests question 2 ***)
let () =
  assert (est_une_vallee [D]);
  assert (not (est_une_vallee [B]));
  assert (not (est_une_vallee [H]));
  assert (not (est_une_vallee [G]));
  assert (est_une_vallee vallee_1);
  assert (est_une_vallee vallee_2);
  assert (est_une_vallee vallee_3);
  assert (est_une_vallee vallee_4);
  assert (not (est_une_vallee [B; B; G; D; B; B]));
  assert (not (est_une_vallee [B; B; D; G; B; B]));
  assert (not (est_une_vallee [G; G; H; B; D; D]));
  assert (not (est_une_vallee [G; G; B; H; D; D]));
  assert (not (est_une_vallee [H; G; G; B; B; D]));
  assert (not (est_une_vallee [G; G; B; B; D; B]));
  assert (not (est_une_vallee [B; B; B; H; H; H]));
  assert (not (est_une_vallee [B; B; B; D; B; D; D; H; D; D; B; D; H; H; H]));
  assert (not (est_une_vallee [B; D; D; D; H; H; D; D; B; B; D; D; H; H]));
  Printf.printf "\nTous les tests de la question 2 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n";;


(*** Question 3 ***)
let voisin ((x, y) : int * int) (d : direction) : int * int =
  let (dx, dy)= match d with 
    | D -> (1, 0)
    | B -> (0, 1)
    | H -> (0, -1)
    | G -> (-1, 0) in 
  (x+dx, y+dy);;


(*** Question 4***)
let liste_des_points (g : direction list) : (int * int) list = 
  let last = ref (0, 0) in
  let points = ref (!last::[]) in 
  let rec aux li = match li with
    | [] -> ()
    | h::t -> points := ((voisin !last h)::!points); last := (voisin !last h); aux t in
  aux g; List.rev !points;;

(*** Tests question 4 ***)
let () =
  let l1 = [(0, 0); (0, 1); (0, 2); (0, 3); (1, 3); (2, 3); (2, 4); (3, 4); (3, 5);
            (3, 6); (3, 7); (4, 7); (5, 7); (5, 8); (6, 8); (7, 8); (8, 8); (8, 7);
            (9, 7); (9, 6); (10, 6); (10, 5); (10, 4); (10, 3); (11, 3); (11, 2);
            (12, 2); (13, 2); (13, 1)] in
  let l2 = [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 4); (2, 4); (2, 3); (3, 3);
            (4, 3); (4, 2); (4, 1); (4, 0); (5, 0); (5, -1); (5, -2); (5, -3)] in
  let l3 = [(0, 0); (0, 1); (0, 2); (0, 3); (1, 3); (1, 4); (1, 5); (1, 6); (2, 6);
            (3, 6); (3, 7); (4, 7); (5, 7); (5, 6); (5, 5); (5, 4); (5, 3)] in
  let l4 = [(0, 0); (0, 1); (0, 2); (0, 3); (1, 3); (2, 3); (2, 4); (3, 4); (4, 4);
            (5, 4); (6, 4); (6, 5); (6, 6); (6, 7); (6, 8); (6, 9); (6, 10); (7, 10);
            (7, 9); (7, 8); (7, 7); (7, 6); (7, 5); (7, 4); (7, 3); (8, 3); (9, 3);
            (10, 3); (11, 3); (12, 3); (12, 2); (13, 2); (14, 2)] in
  assert(liste_des_points [] = [(0, 0)]);
  assert(liste_des_points [B] = [(0, 0); (0, 1)]);
  assert(liste_des_points [H] = [(0, 0); (0, -1)]);
  assert(liste_des_points [D] = [(0, 0); (1, 0)]);
  assert(liste_des_points [G] = [(0, 0); (-1, 0)]);
  assert(liste_des_points [G; B; D; H] = [(0, 0); (-1, 0); (-1, 1); (0, 1); (0, 0)]);
  assert(liste_des_points vallee_1 = l1);
  assert(liste_des_points vallee_2 = l2);
  assert(liste_des_points vallee_3 = l3);
  assert(liste_des_points vallee_4 = l4);
  Printf.printf "\nTous les tests de la question 4 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n";;

(*** Question 5 ***)

(* On peut évaluer la complexité de est_simple à un ordre O(2n) ~ O(n), car on fait deux parcours indépendants de la liste de directions*)

let est_simple (g : direction list) : bool =
  let points = liste_des_points g in
  let hashmap = Hashtbl.create (List.length g) in (* Table de hachage qui sert à répertorier les points déjà rencontrés, les valeurs sont donc ignorées,
  seules les clés importent *)
  let rec aux li = match li with 
    | [] -> true
    | h::t -> if Hashtbl.mem hashmap h then false else begin Hashtbl.add hashmap h 0; aux t end in
  aux points;;

(*** Tests question 5 ***)
let _ =
  assert (est_simple vallee_1);
  assert (est_simple vallee_2);
  assert (est_simple vallee_3);
  assert (est_simple vallee_4);
  assert (est_simple []);
  assert (not (est_simple [G; B; D; H]));
  assert (not (est_simple [B; B; B; D; D; D; H; H; G; G; G; G; G; G]));
  Printf.printf "\nTous les tests de la question 5 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n" ;;


(*** Question 6 ***)
let fond (v : direction list) : int * int =
  assert (est_une_vallee v);
  let is_lower_than (x1, y1) (x2, y2) = y2<y1 in (* < car on veut garder la portion de profil la plus profonde ET la plus à gauche*)
  let bottom = ref (0, 0) in
  let points = liste_des_points v in
  let rec aux li = match li with
    | [] -> () 
    | h::t -> if is_lower_than h !bottom then bottom := h; aux t in
  aux points; !bottom;;

(*** Tests question 6 ***)
let _ =
  assert (fond vallee_1 = (5, 8));
  assert (fond vallee_2 = (0, 4));
  assert (fond vallee_3 = (3, 7));
  assert (fond vallee_4 = (6, 10));
  Printf.printf "\nTous les tests de la question 6 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n";;    


(*** Question 7 ***)
let rec reduce li = match li with 
    | [] -> failwith "unexpected error"
    | _::[] -> []
    | h::t -> h::(reduce t);;

(* plateaux est bien en cpxité linéaire *)
let plateaux (v : direction list) = 
  assert (est_une_vallee v);
  let points = reduce (liste_des_points v) in
  let l = List.combine v points in 
  let pass = ref true in 
  let start = ref (-1, -1) in
  let plates = ref [] in
  let i = ref 0 in

  let aux1 p = 
    assert (!start <> (-1, -1));
    pass := true; 
    let (x1, _) = !start in let (x2, y2) = p in
    start := (-1, -1);
    plates := (x1, x2, y2)::!plates; in

  let rec aux2 li = match li with
    | [] -> ()
    | (d, p)::t -> i:=!i+1; if d = D then 
      begin
        if !i = 0 || !pass then begin
          start := p;
          pass := false; 
        end else if t=[] then begin 
          let (x, y) = p in aux1 (x+1, y);
        end (* Mathematically, atp start != (-1, -1), as one D had to initiate the plate in the past *)
      end else begin
        if !start <> (-1, -1) then aux1 p;
      end; aux2 t in aux2 l; List.rev !plates;;

(*** Tests question 7 ***)
let _ =
  let p1 = [(0, 2, 3); (2, 3, 4); (3, 5, 7); (5, 8, 8); (8, 9, 7); 
            (9, 10, 6); (10, 11, 3); (11, 13, 2)] in
  let p2 = [(0, 2, 4); (2, 4, 3); (4, 5, 0)] in
  let p3 = [(0, 1, 3); (1, 3, 6); (3, 5, 7)] in
  let p4 = [(0, 2, 3); (2, 6, 4); (6, 7, 10); (7, 12, 3); (12, 14, 2)] in
  assert (plateaux vallee_1 = p1);
  assert (plateaux vallee_2 = p2);
  assert (plateaux vallee_3 = p3);
  assert (plateaux vallee_4 = p4);
  Printf.printf "\nTous les tests de la question 7 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n";;

(*** Question 8 ***)
let decomposition_en_rectangles v = 
  let points = liste_des_points v in
  let p = List.stable_sort (fun (_, _, y) (_, _, _y) -> - compare y _y) (plateaux v) in

  let max_width li = 
    let arr = Array.of_list li in 
    let length = Array.length arr in 
    let (x2, _) = arr.(length-1) in 
    let (x1, _) = arr.(0) in 
    abs(x2-x1) in

  let top_width = max_width points in

  let rec aux_core li = 
    match li with
    | [] -> []
    | (x1, x2, y)::[] -> (top_width, -1)::[]
    | (x1, x2, y)::(_x1, _x2, _y)::[] -> if y = _y then (top_width, -1)::[] else (top_width-(_x2-_x1), abs(_y-y))::(top_width, -1)::[]
    | (x1, x2, y)::(_x1, _x2, _y)::(__x1, __x2, __y)::l -> let diff = if compare x1 _x1 <= 0 then (_x2-x1) else (x2-_x1) in
      if y = _y then begin (diff, abs(__y-_y))::aux_core((__x1, __x2, __y)::l) end else if _y = __y then (min (abs(__x2-_x1)) (abs(_x2-__x1)), abs(_y-y))::aux_core((_x1, _x2, _y)::(__x1, __x2, __y)::l) else (diff-(_x2-_x1), abs(_y-y))::aux_core((_x1, _x2, _y)::(__x1, __x2, __y)::l) in
  aux_core p;;

let _ =
  let r1 = [(3, 1); (6, 1); (7, 2); (8, 1); (11, 1); (13, -1)] in
  let r2 = [(2, 1); (4, 3); (5, -1)] in
  let r3 = [(2, 1); (4, 3); (5, -1)] in
  let r4 = [(1, 6); (5, 1); (12, 1); (14, -1)] in
  assert (decomposition_en_rectangles vallee_1 = r1);
  assert (decomposition_en_rectangles vallee_2 = r2);
  assert (decomposition_en_rectangles vallee_3 = r3);
  assert (decomposition_en_rectangles vallee_4 = r4);
  Printf.printf "\nTous les tests de la question 8 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n";;

(*** Question 9 ***)
let hauteur_de_l_eau (t : float) (v : direction list) =
  let rectangles = decomposition_en_rectangles v in

  let rec aux t_remain h_acc = function
    | [] -> h_acc
    | (w, h)::rest -> let wf = float_of_int w in
    if h = -1 then h_acc +. t_remain /. wf else begin
      let hf = float_of_int h in 
      let t_rect = wf *. hf in
      if t_rect>t_remain then h_acc +. t_remain /. wf else hf +. (aux (t_remain-.(wf *. hf)) h_acc rest) end in
  aux t 0. rectangles;;

(*** Tests question 9 ***)
let _ =
  assert (hauteur_de_l_eau 0.0 vallee_1 = 0.0);
  assert (hauteur_de_l_eau 3.0 vallee_1 = 1.0);
  assert (hauteur_de_l_eau 9.0 vallee_1 = 2.0);
  assert (hauteur_de_l_eau 16.0 vallee_1 = 3.0);
  assert (hauteur_de_l_eau 23.0 vallee_1 = 4.0);
  assert (hauteur_de_l_eau 31.0 vallee_1 = 5.0);
  assert (hauteur_de_l_eau 42.0 vallee_1 = 6.0);
  assert (hauteur_de_l_eau 55.0 vallee_1 = 7.0);
  assert (hauteur_de_l_eau 68.0 vallee_1 = 8.0);

  assert (hauteur_de_l_eau 2.0 vallee_2 = 1.0);
  assert (hauteur_de_l_eau 6.0 vallee_2 = 2.0);

  for i=0 to 1000 do
    let t = (float_of_int i /. 10.) in
    assert (hauteur_de_l_eau t vallee_2 = hauteur_de_l_eau t vallee_3)
  done;
  
  assert (hauteur_de_l_eau 0.0 vallee_4 = 0.0);
  assert (hauteur_de_l_eau 1.0 vallee_4 = 1.0);
  assert (hauteur_de_l_eau 2.0 vallee_4 = 2.0);
  assert (hauteur_de_l_eau 3.0 vallee_4 = 3.0);
  assert (hauteur_de_l_eau 4.0 vallee_4 = 4.0);
  assert (hauteur_de_l_eau 5.0 vallee_4 = 5.0);
  assert (hauteur_de_l_eau 6.0 vallee_4 = 6.0);
  assert (hauteur_de_l_eau 11.0 vallee_4 = 7.0);
  assert (hauteur_de_l_eau 23.0 vallee_4 = 8.0);
  assert (hauteur_de_l_eau 37.0 vallee_4 = 9.0);

  Printf.printf "\nTous les tests de la question 9 ont été validés.\n";
  Printf.printf "Vous avez terminé la partie OCaml.\n";;
