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
let bleu_clair = rgb 80 199 199;;
let vert_clair = rgb 38 145 108;;
let rose = rgb 150 47 100;;
let bleu_fonce = rgb 47 39 138;;
let violet = rgb 142 115 222;;
let orange = rgb 212 133 42;;
let bleu_ciel = rgb 42 206 212;;
let la3 = sound 440 1;;

(* Fonctions pour tracer le background (titre, score) *)


let initialisation = 
	open_graph ":0";
	resize_window 800 600;
	set_color rouge_fonce;
	fill_rect 0 0 800 600;
	moveto 35 500;
	set_color white;
  set_text_size 50;
	draw_string "2048";
	moveto 35 300;
	set_text_size 25;
	draw_string "Score :";
	moveto 15 30;
	set_text_size 15;
	draw_string "Made by Elisa";

	for i = 0 to 3 do
		begin
			moveto (200+150*i) 0;
			lineto (200+150*i) 600;
			moveto 200 (150*i);
			lineto 800 (150*i);
		end
	done;;

(* Fonctions de générateurs aléatoires *)
let deux_ou_quatre = if (int 2) = 0 then 2 else 4;;
	
(* problème si on a les mêmes coord *)
let debut array array_prime = let a = int 4 and b = int 4
		and c = int 4 and d = int 4 in
		array.(a).(b) <- deux_ou_quatre;
		array.(c).(d) <- deux_ou_quatre;
		array_prime.(a).(b) <- 1;
		array_prime.(c).(d) <- 1;;
(* Fonctions qui modifie le tableau + score *)

(* Fonctions qui lit le tableau et l'affiche + affiche le background *)
	
let print_score score = 
		set_color white;
		set_text_size 25;
		moveto 35 200;
		draw_string (string_of_int score);;
	
		(* fonctions qui détermine la couleur de chaque case associée 
ainsi que le son entendu lors d'une collision *)

let couleur_case nombre = match nombre with
		| 2 -> rouge
		| 4 -> vert
		| 8 -> yellow
		| 16 -> black
		| 32 -> bleu
		| 64 -> bleu_clair
		| 128 -> rose
		| 256 -> vert_clair
		| 512 -> bleu_fonce
		| 1024 -> violet 
		| 2048 -> orange
		| _ -> bleu_ciel;;
					

		
let son_collision nombre = match nombre with
		| 2 -> la
		| 4 ->
		| 8 -> 
		| 16 -> 
		| 32 -> 
		| 64 ->
		| 128 ->
		| 256 ->
		| 512 -> 
		| 1024 ->
		| 2048 -> 
		| 4096 -> 
		;;
		
		
let taille_nombre nb x y = match nb with
		| n when n < 10 -> moveto (x+60) (y+50)
		| n when n < 100 -> moveto (x+45) (y+50)
		| n when n < 1000 -> moveto (x+25) (y+50)
		| _ -> set_text_size 40; moveto (x+23) (y+55);;
		

	
		(* affiche chaque case + nombre *)
let print_cases x y nombre =
		set_color (couleur_case nombre);
		fill_rect (x+10) (y+10) 130 130;
		set_color white;
		set_text_size 50;
		taille_nombre nombre x y;
		draw_string (string_of_int nombre)
		;;
				

let read_print array array_prime = 
    (* affiche case et nombre*)
    for i = 0 to 3 do 
        for j = 0 to 3 do
            begin 
            if array_prime.(i).(j) = 1
            then print_cases (200 + i*150) (450 - j*150) array.(i).(j)
            else ()
            end
        done
    done
    ;;

let test = let f = make_matrix 4 4 0 and g = make_matrix 4 4 0 in
		debut f g;
		read_print f g;;
		
read_print (make_matrix 4 4 2048) (make_matrix 4 4 1);;

(* Fonctions qui choisit un emplacement libre random et choisit 2 ou 4 *)
		


		(* créé une matrice indiquant les emplacements remplis/vides *)
let emplacements_libres array array_prime = 
		for i = 0 to 3 do	
				for j = 0 to 3 do
						if array.(i).(j) != 0 then array_prime.(i).(j) <- 1
						else array_prime.(i).(j) <- 0
				done
		done
		;;
		
let new_coord a b array array_prime = 
		array.(a).(b) <- deux_ou_quatre;
		array_prime.(a).(b) <- 1;;
		
let new_tile array array_prime= let t = ref true in 
		while !t do
				let a = int 4 and b = int 4 in
				if array_prime.(a).(b) = 0 then new_coord a b array array_prime else ()
		done;;
		
		

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
		let over = ref false and tableau = make_matrix 4 4 0 and tableau_prime = make_matrix 4 4 0 in
				while !over do 
					begin 
					initialisation;
					debut tableau;
					
		
	
    			if array_rempli tableau_prime then print_game_over else ()
					end
    done;;

	(* fonction qui détecte le déplacement du joueur (keyboard) *)

read_key;;

    
  (* 4 directions : Nord Sud Est Ouest *)  
  
type direction = Nord | Sud | Est | Ouest ;;

let trad_direction dir = match dir with
		| Nord -> haut
		| Sud -> bas
		| Est -> droite
		| Ouest -> gauche
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


