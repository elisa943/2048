#load "graphics.cma";;
open Graphics;;
open Random;;
open Array;;
open Char;;

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

(* Fonctions de générateurs aléatoires *)
let deux_ou_quatre = if (int 2) = 0 then 2 else 4;;
	
(* Fonctions qui détermine la couleur de chaque case associée 
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
		
type direction = Haut | Bas | Gauche | Droite | Rien;;

let conversion_ASCII_direction n = match n with 
		| 90 -> Haut
		| 122 -> Haut
		| 81 -> Gauche
		| 113 -> Gauche
		| 83 -> Bas
		| 115 -> Bas
		| 68 -> Droite
		| 100 -> Droite
		| _ -> Rien;;

(* Fonctions pour tracer le background (titre, score) *)

let initialisation n = 
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


(* problème si on a les mêmes coord *)
let debut array array_prime = let a = int 4 and b = int 4
		and c = int 4 and d = int 4 in
		array.(a).(b) <- deux_ou_quatre;
		array.(c).(d) <- deux_ou_quatre;
		array_prime.(a).(b) <- 1;
		array_prime.(c).(d) <- 1;;
(* Fonctions qui modifie le tableau + score *)

(* Fonctions qui lit le tableau et l'affiche + affiche le background *)
	
		(* affiche le score *)
let print_score score = 
		set_color white;
		set_text_size 25;
		moveto 35 200;
		draw_string (string_of_int score);;
		
		(* adapte la position du nombre selon la taille qu'il prend *)
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

		(* lit les tableaux, détermine ce qui doit être affiché
		et appelle la fonction print_cases *)
let read_print_cases array array_prime = 
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
		
read_print_cases (make_matrix 4 4 2048) (make_matrix 4 4 1);;

(* Fonctions qui choisit un emplacement libre random et choisit 2 ou 4 *)
		


		(* update array_prime -> pas forcément nécessaire *)
let update array array_prime = 
		for i = 0 to 3 do	
				for j = 0 to 3 do
						if array.(i).(j) != 0 then array_prime.(i).(j) <- 1
						else array_prime.(i).(j) <- 0
				done
		done
		;;
		
		(* choisit de façon random un emplacement libre *)
		
let new_tile array array_prime = let boo = ref true in
		while !boo do
				let a = int 4 and b = int 4 in
				if array_prime.(a).(b) = 0 
						then 
							array.(a).(b) <- deux_ou_quatre;
							array_prime.(a).(b) <- 1;
							boo := false
		done;;

(* Fonctions qui détecte le game over *)

let condition_vide element = element = 1;;

		(* renvoie true si le tableau est rempli = game over *)
let array_rempli array_prime = 
		for_all condition_vide array_prime;;
		
		(* renvoie true si le joueur n'a plus de choix possible 
		A TERMINER *)
let bloque array = 
		for i = 0 to 3 do 
				for j = 0 to 3 do 
						if array.(i).(j) = array.(i).(j+1) then false
						else if array.(j).(i) = array.(j).(i+1) then false;
						
				done
		done
		true;;
		

(* Fonction qui affiche l'écran de game over *)

let print_game_over n = 
    set_color vert;
    fill_rect 300 200 360 200;
		moveto 330 280;
    set_text_size 50;
    set_color white;
    draw_string "GAME OVER";;
    
(* Fonction qui affiche un bouton "PLAY AGAIN" 
		A TERMINER*)

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
		then initialisation 0 else ();;



(* Fonction qui détecte le déplacement du joueur (keyboard) *)
let read_clavier = code (read_key());;

let output_direction dir array array_prime = match dir with 
		|	Haut -> haut array array_prime
		| Bas -> bas array array_prime
		| Gauche -> gauche array array_prime
		| Droite -> droite array array_prime
		| Rien -> ();;
		
let detecte_deplacement array array_prime = 
		if key_pressed() then output_direction read_clavier array array_prime
		else ();;

let etat alpha beta a b = match alpha, beta with 
		| 1, 0 -> array.(i).(j-1) <- array.(i).(j-1);
							array.(i).(j-1) <- 0;
							array_prime.(i).(j-1) <- 1;
							array_prime.(i).(j) <- 0

		| x, y when a = b -> array.(i).(j-1) <- array.(i).(j-1)*array.(i).(j-1)
													array.(i).(j-1) <- 0;
													array_prime.(i).(j) <- 0;

		| _ -> ();;
		

  
let haut array array_prime = 
		for j = 1 to 3 do
				for i = 0 to 3 do 
						k = etat array_prime.(i).(j-1) array_prime.(i).(j) array.(i).(j) array.(i).(j-1);
				done
		done;;
    
let bas array array_prime = 
		for j = 2 downto 0 do
				for i = 0 to 3 do 
				
				done;
		done;;
		
let gauche array array_prime = 
		for i = 2 downto 0 do
				for j = 0 to 3 do 
				
				done
		done;;
		
let droite array array_prime = 
		for j = 2 downto 0 do
				for i = 0 to 3 do 
				
				done
		done;;
	

let f =
	while true do 
		read_key;;

let test = let boo = ref true in
	while boo do
	if Key_pressed = true 
    	then read_key
		else "over"
    done
;;

(* ------------------ ZONE DE TEST ------------------*)


read_key();;
key_pressed;;

Char.code 'Z';;
Char.code 'q';;
Char.code 's';;
Char.code 'd';;

(* ------------------ ZONE DE TEST -------------------*)

(* Fonction main *)

let g = let continuer = ref true and tableau = make_matrix 4 4 0 
				and tableau_prime = make_matrix 4 4 0 in
						while !continuer do
							begin
								initialisation 0;
								debut tableau tableau_prime;
								read_print_cases tableau tableau_prime;
								continuer := false;
							end;
						done;;
								