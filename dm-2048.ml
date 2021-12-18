#load "graphics.cma";;

open Graphics;;
open Random;;
open Array;;
open List;;

(* https://htmlcolors.com/google-color-picker *)
(* https://caml.inria.fr/pub/docs/manual-caml-light/node16.html
	 https://musicordes.fr/tableau-frequences-notes/
*)

open_graph ":0";;

resize_window 800 600;;

(* Fonctions pour tracer le background (titre, score) *)

let bleu = rgb 38 92 153;;
let rouge = rgb 224 20 64;;
let vert = rgb 27 133 3;;
let orange = rgb 222 118 20;;
let la3 = sound 440 1;;



let background = 
	set_color bleu;
	fill_rect 0 0 800 600;
	moveto 50 500;
	set_color rouge;
  set_text_size 50;
	draw_string "2048";
	moveto 50 300;
	set_text_size 30;
	draw_string "Score :";
	for i = 1 to 4 do
		begin
			moveto (199*i) 0;
			lineto (199*i) 600;
		end
	done;;
	
	
	
	

let point x y nb = 
	(* sert à obtenir un certain paramètre d'un couple de coordonnées *)
	if nb = 0 then x else y;;




close_graph;;
clear_graph;;

(* Structures pour les blocs : sous la forme d'un tableau (listes) *)

let cases = make_matrix 4 4 0;;

(* Fonctions qui modifie le tableau + score *)

(* Fonctions qui lit le tableau et l'affiche + affiche le background *)

	(* tracer les lignes et contours, ajouter la couleur, les blocs, les nombres *)

(* Fonctions qui choisit un emplacement libre random et choisit 2 ou 4 *)

(* Fonctions qui détecte le game over *)

(* Fonction qui affiche l'écran de game over *)

let array_vide array = 
	for i = 0 to 3 do	
		for j = 0 to 3 do
			if !array.(i).(j) != 0 then false 
		done;
	done;
;;

let game_over = clear_graph;;

(* Fonction main *)


let game = 
	let over = ref false 
		and tableau = ref make_matrix 4 4 0 in
	while !over do 
	begin 
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


