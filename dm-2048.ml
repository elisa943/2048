#load "graphics.cma";;

open Graphics;;
open Random;;
open Array;;

(* https://htmlcolors.com/google-color-picker *)
(* https://caml.inria.fr/pub/docs/manual-caml-light/node16.html
	 https://musicordes.fr/tableau-frequences-notes/
*)

(* 150x150 *)

open_graph ":0";;

resize_window 800 600;;

(* Fonctions pour tracer le background (titre, score) *)

let bleu = rgb 38 92 153;;
let rouge = rgb 224 20 64;;
let vert = rgb 27 133 3;;
let rouge_fonce = rgb 135 42 8;;

let la3 = sound 440 1;;



let background = 
	set_color rouge_fonce;
	fill_rect 0 0 800 600;
	moveto 15 500;
	set_color white;
  set_text_size 50;
	draw_string "2048";
	moveto 25 300;
	set_text_size 25;
	draw_string "Score :";
	moveto 15 30;
	set_text_size 15;
	draw_string "Made by Elisa";

	moveto 200 0;
	for i = 1 to 4 do
		begin
			moveto (160*i) 0;
			lineto (160*i) 600;
			moveto 160 (150*i);
			lineto 800 (150*i);
		end
	done;;
	
	

let point x y nb = 
	(* sert à obtenir un certain paramètre d'un couple de coordonnées *)
	if nb = 0 then x else y;;




close_graph;;
clear_graph;;

(* Structures pour les blocs : sous la forme d'un tableau (listes) *)



(* Fonctions qui modifie le tableau + score *)

(* Fonctions qui lit le tableau et l'affiche + affiche le background *)


let read_and_print array = 
	set_text_size 10;
	
	for i = 0 to 3 do
		for j = 0 to 3 do
			begin
			moveto x y ;
			set_color shade;
			fill_rect x y w h
			set_color shade;
			draw_string char_of_int array.(i).(j);
			end
		done
	done;;
	

	(* tracer les lignes et contours, ajouter la couleur, les blocs, les nombres *)

(* Fonctions qui choisit un emplacement libre random et choisit 2 ou 4 *)

let deux_ou_quatre = if (int 2) = 0 then 2 else 4;;

let emplacements_libres array = 
	let array_prime = make_matrix 4 4 0 in
	for i = 0 to 3 do	
		for j = 0 to 3 do
			if array.(i).(j) != 0 then array_prime.(i).(j) <- 1
			else array_prime.(i).(j) <- 0
		done;
	done;
	array_prime;;

(* Fonctions qui détecte le game over *)

let array_vide array_prime = let vide = ref false and boo = ref true in
	while !boo do
	begin
	for i = 0 to 3 do	
		for j = 0 to 3 do
			if array_prime.(i).(j) <> 0 then boo := false 
			else boo := true
		done;
	done;
	end;
	!vide;;

(* Fonction qui affiche l'écran de game over *)



let game_over = clear_graph;;

(* Fonction main *)


let game = 
	let over = ref false 
	and tableau = make_matrix 4 4 0 in
	while !over do 
	begin 
		if Key_pressed 
		then fleche = ref read_key 
		else fleche = ref Null
		
		if read 
		
	
	
	
	
    if array_vide tableau then 3 else 4
	end
    done;;

	(* fonction qui détecte le déplacement du joueur (keyboard) *)
    
Key_pressed;; 
read_key;;

let ff = while true do
	read_key;;

let test = let boo = ref true in
while boo do
	if Key_pressed = true 
    	then read_key
		else "over"
    done;
;;


