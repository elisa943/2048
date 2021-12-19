#load "graphics.cma";;
open Graphics;;
open Random;;
open Array;;

(* 
https://htmlcolors.com/google-color-picker
https://caml.inria.fr/pub/docs/manual-caml-light/node16.html
https://musicordes.fr/tableau-frequences-notes/
*)

(* 160x150 *)

(* Variables préliminaires *)

let bleu = rgb 38 92 153;;
let rouge = rgb 224 20 64;;
let vert = rgb 27 133 3;;
let rouge_fonce = rgb 135 42 8;;

let la3 = sound 440 1;;

(* Fonctions pour tracer le background (titre, score) *)


let background = 
	open_graph ":0";
	resize_window 800 600;
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

fill_rect 0 0 160 150;;	
	

let point x y nb = 
	(* sert à obtenir un certain paramètre d'un couple de coordonnées *)
	if nb = 0 then x else y;;


close_graph;;
clear_graph;;

(* Structures pour les blocs : sous la forme d'un tableau (listes) *)



(* Fonctions qui modifie le tableau + score *)

(* Fonctions qui lit le tableau et l'affiche + affiche le background *)
	
	
		(* affiche chaque case + nombre *)
let print_cases x y nombre =
		moveto x y;
		set_color bleu;
		fill_rect x y 140 130;
		moveto (x+55) (y+45);
		set_color white;
		draw_string (string_of_int nombre)
		;;

let read_print array array_prime = 
    (* affiche case et nombre*)
    set_text_size 50;
    for i = 0 to 3 do 
        for j = 0 to 3 do
            begin 
            if array_prime.(i).(j) = 1
            then print_cases (150+150*(i mod 4)+20) (450-150*(j mod 4)+10) array.(i).(j)
            else ()
            end
        done
    done
    ;;

read_print (make_matrix 4 4 8) (make_matrix 4 4 1);;

(* Fonctions qui choisit un emplacement libre random et choisit 2 ou 4 *)

let deux_ou_quatre = if (int 2) = 0 then 2 else 4;;

		(* créé une matrice indiquant les emplacements remplis/vides *)
let emplacements_libres array = 
		let array_prime = make_matrix 4 4 0 in
		for i = 0 to 3 do	
				for j = 0 to 3 do
						if array.(i).(j) != 0 then array_prime.(i).(j) <- 1
						else array_prime.(i).(j) <- 0
				done
		done
		;;

(* Fonctions qui détecte le game over *)

let condition_vide element = element = 1;;

		(* renvoie true si le tableau est rempli = game over *)
let array_rempli array_prime = 
		for_all condition_vide array_prime;;


(* Fonction qui affiche l'écran de game over *)

let print_game_over = 
    set_color vert;
    fill_rect 300 200 360 200;
		moveto 330 280;
    set_text_size 50;
    set_color white;
    draw_string "GAME OVER";;
    
(* Fonction qui affiche un bouton "PLAY AGAIN" *)

wait_next_event;;

let detecte_play_again x y w h etat= 
	x <= etat.mouse_x && etat.mouse_x >= x+w 
	&& y <= etat.mouse_y && etat.mouse_y >= y+h && etat.button;;

let play_again = 
		set_color vert;
		fill_rect 330 50 300 100;
		moveto 370 90;
		set_text_size 30;
		set_color white;
		draw_string "PLAY AGAIN";
		if detecte_play_again 330 50 300 100 (wait_next_event Poll)
		then background else ();;


(* Fonction main *)


let game = 
		let over = ref false and tableau = make_matrix 4 4 0 in
				while !over do 
					begin 
					background;
					
		
	
    				if array_repli tableau then  else 4
					end
    done;;

	(* fonction qui détecte le déplacement du joueur (keyboard) *)

read_key;;

    
  (* 4 directions : Nord Sud Est Ouest *)  
  
type direction = Nord | Sud | Est | Ouest ;;

let trad_direction dir = match dir with
		| Nord -> 
		| Sud -> 
		| Est -> 
		| Ouest -> 
;;	

let read_dir = 
		if key_pressed() then read_key
		else 'A;;

		  
  
Key_pressed;; 
read_key;;

let test = let boo = ref true in
while boo do
	if Key_pressed = true 
    	then read_key
		else "over"
    done
;;


