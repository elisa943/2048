open Graphics;;
open Random;;
open List;;

(* https://htmlcolors.com/google-color-picker *)
(*https://caml.inria.fr/pub/docs/manual-caml-light/node16.html*)

open_graph "";;

resize_window 800 600;;

(* Fonctions pour tracer le background (titre, score) *)

let bleu = rgb 15 140 171;;

let rouge = rgb 224 20 64;;

let titre = 
	moveto 50 500;
	set_color bleu;
    set_text_size 50;
	set_font "Avenir Next";
	draw_string "2048";;

let point x y nb = 
	(* sert à obtenir un certain paramètre d'un couple de coordonnées *)
	if nb = 0 then x else y;;

let lignes = 
	set_line_width = 600;
    moveto 200 0;
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

(* Fonctions qui modifie le tableau + score *)

(* Fonctions qui lit le tableau et l'affiche + affiche le background *)

	(* tracer les lignes et contours, ajouter la couleur, les blocs, les nombres *)

(* Fonctions qui choisit un emplacement libre random et choisit 2 ou 4 *)

(* Fonctions qui détecte le game over *)

(* Fonction qui affiche l'écran de game over *)


(* Fonction main *)

	(* fonction qui détecte le déplacement du joueur (keyboard) *)
    
key_pressed;;

read_key;;

sound;;
    
    
    (* fonction *)
