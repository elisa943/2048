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

(* Variables préliminaires *)
open_graph ":0";;
resize_window 800 600;;
	
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

let do3 = sound 131 1;;
let re3 = sound 294 1;;
let mi3 = sound 330 1;;
let fa3 = sound 349 1;;
let sol3 = sound 392 1;;
let la3 = sound 440 1;;
let si3 = sound 494 1;;

let do4 = sound 523 1;;
let re4 = sound 587 1;;
let mi4 = sound 659 1;;
let fa4 = sound 698 1;;
let sol4 = sound 784 1;;

(* ---------- Fonctions de générateurs aléatoires ---------- *)
let deux_ou_quatre = if (int 2) = 0 then 2 else 4;;

(* Fonctions qui détermine :
- la couleur de chaque case associée 
- le son entendu lors d'une collision 
- la direction indiquée par le joueur sur le clavier *)

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
		| 2 -> do3
		| 4 -> re3
		| 8 -> mi3
		| 16 -> fa3
		| 32 -> sol3
		| 64 -> la3
		| 128 -> si3
		| 256 -> do4
		| 512 -> re4
		| 1024 -> mi4
		| 2048 -> fa4
		| _ -> sol4
		;;
		
type direction = Haut | Bas | Gauche | Droite | Rien;;

		(* 
		Z ou z -> haut
		Q ou q -> gauche 
		S ou s -> bas
		D ou d -> droite
		 *)
let direction_of_ASCII d = match d with 
		| 90 -> Haut
		| 122 -> Haut
		| 81 -> Gauche
		| 113 -> Gauche
		| 83 -> Bas
		| 115 -> Bas
		| 68 -> Droite
		| 100 -> Droite
		| _ -> Rien;;

(* ---------- Fonctions pour tracer le background (titre, score) ---------- *)

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

(* NB : problème si on a les mêmes coord (mêmes si les proba sont faibles *)
let debut array array_prime = let a = int 4 and b = int 4
		and c = int 4 and d = int 4 in
		array.(a).(b) <- deux_ou_quatre;
		array.(c).(d) <- deux_ou_quatre;
		array_prime.(a).(b) <- 1;
		array_prime.(c).(d) <- 1;;
		
		
		(* affiche le score *)
let print_score score = 
		set_color white;
		set_text_size 25;
		moveto 35 200;
		draw_string (string_of_int score);;

(* ---------- Fonctions qui lit le tableau, l'affiche ---------- *)
	

		
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

(* ---------- Fonctions qui choisissent un emplacement libre random et ajoute 2 ou 4 ---------- *)
		
		(* update array_prime en fonction de array  *)
let update array array_prime = 
		for i = 0 to 3 do	
				for j = 0 to 3 do
						if array.(i).(j) != 0 then array_prime.(i).(j) <- 1
						else array_prime.(i).(j) <- 0
				done
		done
		;;
		
		(* choisit de façon random un emplacement libre 
		et y ajoute de façon random une case entre 2 et 4 *)
let new_tile array array_prime = let boo = ref true in
		while !boo do
				let a = int 4 and b = int 4 in
				if array_prime.(a).(b) = 0 
						then 
							array.(a).(b) <- deux_ou_quatre;
							update array array_prime;
							boo := false
		done;;

(* ---------- Fonctions qui détecte le game over ---------- *)

		(* fonction auxiliaire utilisée dans la fonction suivante *)
let condition_vide element = element = 1;;

		(* renvoie true si le tableau est rempli 
		NB : Utiliser let rec pour que ça soit plus beau ?
		*)
let array_rempli array_prime = 
		for_all condition_vide array_prime.(0) && 
		for_all condition_vide array_prime.(1) &&
		for_all condition_vide array_prime.(2) &&
		for_all condition_vide array_prime.(3);;
		

		(* renvoie true si le joueur n'a plus de choix possible *)

		(* fonction auxiliaire qui renvoie true s'il existe 
		deux mêmes éléments d'affilée *)
let deux_daffilee liste = let tmp = ref false in
		for i = 0 to 2 do
				if liste.(i) = liste.(i+1) then tmp := true 
					else ()
		done;
		!tmp;;
				
		(* fonction qui :
		- utilise deux_d'affilee sur les lignes. 
		- crée une nouvelle matrice 4x4 en inversant lignes et 
		colonnes puis utilise deux_d'affilee sur les nouvvelles
		lignes. 
		
		NB : il existe sûrement un moyen plus rapide pour réaliser
		cette fonction.
		
		*)
let bloque array = let var = ref false in
		for i = 0 to 3 do 
				if deux_daffilee array.(i) = false 
					then var := true 
				else ()
		done;
		
		if !var then !var else 
				let array_inv = make_matrix 4 4 0 in 
						for i = 0 to 3 do 
								for j = 0 to 3 do
									array_inv.(i).(j) <- array.(j).(i);
								done;
							done;
						for i = 0 to 3 do 
								if deux_daffilee array.(i) = false 
									then var := true 
								else ()
						done;
						!var
				;;
		

(* ---------- Fonction qui affiche l'écran de game over ---------- *)

let print_game_over n = 
    set_color vert;
    fill_rect 300 200 360 200;
		moveto 330 280;
    set_text_size 50;
    set_color white;
    draw_string "GAME OVER";;
    
(* ---------- Fonction qui affiche un bouton "PLAY AGAIN" ---------- 
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



(* ---------- Fonctions qui détecte le déplacement du joueur---------- *)

		(* Les 4 fonctions suivantes fonctionnent de manière analogue :
		On définit une variable compteur qui sert à compter le nombre de changements 
		(nécessaire pour stopper la boucle while). Lorsque le compteur atteint 12, cela 
		veut dire qu'il n'y a pas eu de changements durant ces deux boucles for, et donc
		que l'on peut sortir de la boucle while. 
		*)

let haut array array_prime score = 
		let compteur = ref 0 in
		while ! compteur <> 12 do
			begin
				compteur := 0;
				for j = 1 to 3 do
						for i = 0 to 3 do 
								if array.(i).(j) <> array.(i).(j+1) then incr compteur
								else 
								array.(i).(j+1) <- array.(i).(j+1)+array.(i).(j+1);
								score := !score + array.(i).(j+1);
								array.(i).(j) <- 0;
								update array array_prime;
						done;
				done;
			end;
		done;;
		
let bas array array_prime score = 
		let compteur = ref 0 in
		while !compteur <> 12 do
			begin
				compteur := 0;
				for j = 2 downto 0 do
						for i = 0 to 3 do 
								if array.(i).(j) <> array.(i).(j-1) then incr compteur
								else 
									array.(i).(j-1) <- array.(i).(j-1)+array.(i).(j);
									score := !score + array.(i).(j-1);
									array.(i).(j) <- 0;
									update array array_prime;
					done;
				done;
				end;
		done;;
		
let gauche array array_prime score = 
		let compteur = ref 0 in
		while !compteur <> 12 do
			begin
				compteur := 0;
				for i = 1 to 3 do
						for j = 0 to 3 do 
								if array.(i).(j) <> array.(i-1).(j) then incr compteur
								else 
									array.(i-1).(j) <- array.(i-1).(j)+array.(i).(j);
									score := !score + array.(i-1).(j);
									array.(i).(j) <- 0;
									update array array_prime;
						done;
				done;
			end;
		done;;
		
let droite array array_prime score = 
		let compteur = ref 0 in
		while !compteur <> 12 do
			begin
				compteur := 0;
				for j = 2 downto 0 do
						for i = 0 to 3 do 
								if array.(i).(j) <> array.(i+1).(j) then incr compteur
								else
									array.(i+1).(j) <- array.(i+1).(j)+array.(i).(j);
									score := !score + array.(i+1).(j);
									array.(i).(j) <- 0;
									update array array_prime;
						done;
				done;
			end;
		done;;


		(* lit le clavier, convertit l'ASCII  *)
let read_clavier = direction_of_ASCII (code (read_key()));;

		(* utilise la fonction précédente et appelle les 4 fonctions de déplacements 
		précédentes pour faire déplacer les cases *)
let output_direction dir array array_prime score = match dir with 
		|	Haut -> haut array array_prime score
		| Bas -> bas array array_prime score
		| Gauche -> gauche array array_prime score
		| Droite -> droite array array_prime score
		| Rien -> ();;
		
		(* détecte si une touche du clavier est appuyée *)
let detecte_deplacement array array_prime score = 
		if key_pressed() 
				then 
					output_direction read_clavier array array_prime score;
					new_tile array array_prime;;

(* ------------------ ZONE DE TEST ------------------*)






(* ------------------ ZONE DE TEST -------------------*)

(* ---------- Fonction main ---------- *)

let g = let continuer = ref true in
				let tableau = make_matrix 4 4 0 in
				let tableau_prime = make_matrix 4 4 0 in
				let	score = ref 0 in
						
						initialisation 0;
						debut tableau tableau_prime;
						read_print_cases tableau tableau_prime;	
						
						while !continuer do
								if key_pressed() 
									then 
										output_direction read_clavier tableau tableau_prime score;
										new_tile tableau tableau_prime;
								(*detecte_deplacement tableau tableau_prime score;*)
								if array_rempli tableau_prime && bloque tableau
									then print_game_over 0;
								continuer := false;
						done;;							