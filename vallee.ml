type direction = B | H | D | G

(* Définition de quelques vallées pour les tests*)
let vallee_1 = [B; B; B; D; D; B; D; B; B; B; D; D; B; D;
                D; D; H; D; H; D; H; H; H; D; H; D; D; H]
let vallee_2 = [B; B; B; B; D; D; H; D; D; H; H; H; D; H; H; H]
let vallee_3 = [B; B; B; D; B; B; B; D; D; B; D; D; H; H; H; H]
let vallee_4 = [B; B; B; D; D; B; D; D; D; D; B; B; B; B; B; B; 
                D; H; H; H; H; H; H; H; D; D; D; D; D; H; D; D]


(*** Question 1 ***)

let est_sans_rebroussement (g : direction list) : bool =
  if list_len l = 1 then true
  let forbidden = ref H in
  let rec aux li = first := false; match li with
    | [] -> true
    | h::t -> forbidden := match h with
      | B -> H
      | H -> B
      | D -> G
      | G -> D;
      h <> !forbidden in 
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
  Printf.printf "Vous pouvez passer à la question suivante.\n";





(*** Question 2 ***)
let est_une_vallee (g : direction list) : bool =
  failwith "à compléter";

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
  Printf.printf "Vous pouvez passer à la question suivante.\n" in 





(*** Question 3 ***)
let voisin ((x, y) : int * int) (d : direction) : int * int =
  failwith "à compléter";





(*** Question 4 ***)
let liste_des_points (g : direction list) : (int * int) list = 
  failwith "à compléter";
  

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
  Printf.printf "Vous pouvez passer à la question suivante.\n" in




(*** Question 5 ***)
let est_simple (g : direction list) : bool =
  failwith "à compléter";

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
  Printf.printf "Vous pouvez passer à la question suivante.\n" ;
  









(******************************************************************************)
(********************************** Partie 2 **********************************)
(******************************************************************************)

(*** Question 6 ***)
let fond (v : direction list) : int * int =
  failwith "à compléter";
    

(*** Tests question 6 ***)
let _ =
  assert (fond vallee_1 = (5, 8));
  assert (fond vallee_2 = (0, 4));
  assert (fond vallee_3 = (3, 7));
  assert (fond vallee_4 = (6, 10));
  Printf.printf "\nTous les tests de la question 6 ont été validés.\n";
  Printf.printf "Vous pouvez passer à la question suivante.\n" in
  

(*** Question 7 ***)
let plateaux (v : direction list) = 
  failwith "à compléter";

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
  Printf.printf "Vous pouvez passer à la question suivante.\n" in
  




(*** Question 8 ***)
let decomposition_en_rectangles (v : direction list) = 
  failwith "à compléter";



(*** Tests question 8 ***)
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
  Printf.printf "Vous pouvez passer à la question suivante.\n" in


(*** Question 9 ***)
let hauteur_de_l_eau (t : float) (v : direction list) =
  failwith "à compléter";

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
  Printf.printf "Vous avez terminé la partie OCaml.\n";





(******************************************************************************)
(************************************ Bonus ***********************************)
(******************************************************************************)

(*** 
Si vous avez terminé toutes les questions, vous pouvez afficher 
une vallée inondée avec cette fonction. Pour l'utiliser :
- v : est une vallée
- hauteur : représente le niveau de l'eau depuis le fond de la vallée

Inutile d'essayer de comprendre cette fonction !
***)
(*let dessiner_vallee (v : direction list) (hauteur : float) =
  let rec calcul_bordures p min_x max_x min_y max_y =
    match p with
    | [] -> (min_x, max_x, min_y, max_y)
    | (x, y) :: p' -> calcul_bordures p' (min min_x x) (max max_x x) (min min_y y) (max max_y y)
  in
  let (xf, yf) = fond v in
  let points = liste_des_points v in
  let min_x, max_x, min_y, max_y = calcul_bordures points xf xf yf yf in
  let grille = Array.make_matrix (max_x - min_x + 2) (max_y - min_y + 1) true in
  
  List.iter (fun (x0, x1, y') -> 
    for y = 0 to y'-min_y-1 do
      for x = x0+1 to x1 do
        grille.(x).(y) <- false
      done
    done
  ) (plateaux v);

  for y = 0 to max_y-min_y do
    for x = min_x to max_x+1 do
      if grille.(x).(y) then
        Printf.printf "▓▓"
      else begin
        let h' = hauteur -. float (yf-y-min_y-1) in
        if h' >= 1.0 then Printf.printf "██"
        else if h' >= 0.875 then Printf.printf "▇▇"
        else if h' >= 0.750 then Printf.printf "▆▆"
        else if h' >= 0.625 then Printf.printf "▅▅"
        else if h' >= 0.500 then Printf.printf "▄▄"
        else if h' >= 0.375 then Printf.printf "▃▃"
        else if h' >= 0.250 then Printf.printf "▂▂"
        else if h' >= 0.125 then Printf.printf "▁▁"
        else Printf.printf "  "
      end
    done;
    Printf.printf "\n";
  done *)