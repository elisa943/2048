open Graphics;;
open Random;;
open Array;;
open List;;

(* https://htmlcolors.com/google-color-picker *)
(*https://caml.inria.fr/pub/docs/manual-caml-light/node16.html*)

open_graph "";;

resize_window 800 600;;

(* Fonctions pour tracer le background (titre, score) *)

let bleu = rgb 15 140 171;;
let rouge = rgb 224 20 64;;
let la3 = sound 440 1;;


let titre = 
	moveto 50 500;
	set_color rouge;
    set_text_size 50;
	set_font "Avenir Next";
	draw_string "2048";;

let point x y nb = 
	(* sert à obtenir un certain paramètre d'un couple de coordonnées *)
	if nb = 0 then x else y;;

let f = 
let ff = ref 3 in !ff;;



let lignes = 
	let set_line_width = 600 and moveto 200 0 in
    for i = 1 to 4 do
    	begin
	    lineto 200*i (point current_point 1 + set_line_width);
        moveto 200*i+200 (point current_point 1 + 200);
        end
    done;;
    

;;


close_graph;;
clear_graph;;

(* Structures pour les blocs : sous la forme d'un tableau (listes) *)

let cases = make_matrix 4 4 0;;
let detecte


let f = 4
;;
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

let test = let boo = ref true in
while boo do
	if Key_pressed = true 
    	then read_key
		else "over"
    done;
;;


