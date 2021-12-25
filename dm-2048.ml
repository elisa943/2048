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
let jaune = rgb 214 206 49;;
let bleu_marine = rgb 43 66 158;;

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
let deux_ou_quatre n = if (int 2) = 0 then 2 else 4;;

(* Fonctions qui détermine :
- la couleur de chaque case associée 
- le son entendu lors d'une collision 
- la direction indiquée par le joueur sur le clavier *)

let couleur_case nombre = match nombre with
		| 0 -> rouge_fonce
		| 2 -> rouge
		| 4 -> vert
		| 8 -> jaune
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

let direction_of_char c = match c with
		| 'z' -> Haut
		| 'Z' -> Haut
		| 'q' -> Gauche
		| 'Q' -> Gauche
		| 's' -> Bas
		| 'S' -> Bas
		| 'd' -> Droite
		| 'D' -> Droite
		| _ -> Rien;;
		
		
(* ---------- Fonctions pour tracer le background (ce qui reste immobile du début à la fin) ---------- *)

let initialisation n = 
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

(* fonction nécessaire pour être certain d'avoir des coordonnées
différentes c'est-à-dire ne pas avoir seulement une case qui 
apparaisse *)

let verification a b c d = if a = c && b = d then false else true;;

let debut array = let continuer = ref true in
		while !continuer do
				let a = int 4 and
				b = int 4 and
				c = int 4 and
				d = int 4 in
				if verification a b c d then
					begin
					array.(a).(b) <- deux_ou_quatre 0;
					array.(c).(d) <- deux_ou_quatre 0;
					continuer := false
					end
		done;;
		


(* ---------- Fonctions qui lit le tableau, l'affiche ---------- *)
	
		(* déplace le curseur veres la position du nombre (selon la taille du nombre) *)
let taille_nombre nb x y = match nb with
		| n when n < 10 -> moveto (x+60) (y+50)
		| n when n < 100 -> moveto (x+45) (y+50)
		| n when n < 1000 -> moveto (x+25) (y+50)
		| _ -> set_text_size 40; moveto (x+23) (y+55);;


		(* lit les tableaux, détermine ce qui doit être affiché
		et affiche le score  *)
		
let affichage array score = 
    (* affiche le score *)
    set_color rouge_fonce;
		fill_rect 35 250 100 40;
		set_color white;
		set_text_size 25;
		moveto 35 250;
		draw_string (string_of_int !score);
		
		(*affiche les cases et les nombres associés *)
    for i = 0 to 3 do 
        for j = 0 to 3 do
            if array.(i).(j) > 0
            then 
            	begin
            		(* trace la case *)
								set_color (couleur_case array.(i).(j));
								fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
								
								(* trace le nombre *)
								set_color white;
								set_text_size 50;
								taille_nombre array.(i).(j) (200 + i*150) (450 - j*150);
								draw_string (string_of_int array.(i).(j));
            	end
        done
    done;;

(* ---------- Fonction qui choisit un emplacement libre random et ajoute 2 ou 4 ---------- *)

		
		(* choisit de façon random un emplacement libre 
		et y ajoute de façon random 2 ou 4 *)
let new_tile array = let boo = ref true in
		while !boo do
				let a = int 4 and b = int 4 in
				if array.(a).(b) = 0 
						then 
							begin
							array.(a).(b) <- deux_ou_quatre 0;
							boo := false
							end
		done;;

(* ---------- Fonctions qui détecte le game over ---------- *)

		(* fonction auxiliaire utilisée dans la fonction suivante *)
let condition_vide element = element > 0;;

		(* renvoie true si le tableau est rempli 
		NB : Utiliser let rec pour que ça soit plus beau ?
		*)
let array_rempli array = 
		for_all condition_vide array.(0) && 
		for_all condition_vide array.(1) &&
		for_all condition_vide array.(2) &&
		for_all condition_vide array.(3);;
		

		(* renvoie true si le joueur n'a plus de choix possible *)

		(* fonction auxiliaire qui renvoie true s'il existe 
		deux mêmes éléments d'affilée *)
let deux_daffilee liste = 
		let tmp = ref liste.(0) in
		let boo = ref false in
		for i = 1 to 3 do
				if !tmp = liste.(i) then boo := true 
					else tmp := liste.(i)
		done;
		!boo;;
				
		(* fonction qui :
		- utilise deux_d'affilee sur les lignes. 
		- crée une nouvelle matrice 4x4 en inversant lignes et 
		colonnes puis utilise deux_d'affilee sur les nouvvelles
		lignes. 
		
		NB : il existe sûrement un moyen plus rapide pour réaliser
		cette fonction.
		
		*)
let bloque array = 
		let var = ref false in
		for i = 0 to 3 do 
				if deux_daffilee array.(i) = false 
					then var := true 
		done;
		
		if !var then !var else 
				let tmp = ref array.(0).(0) in
				for i = 0 to 3 do
						for j = 1 to 3 do
								if !tmp = array.(i).(j) then var := true
								else tmp := array.(i).(j)
						done;
				done;
				!var;;
				
(* ---------- Fonctions qui détecte le déplacement du joueur---------- *)

		(* Les 4 fonctions suivantes fonctionnent de manière analogue :
		On définit une variable compteur qui sert à compter le nombre de changements 
		(nécessaire pour stopper la boucle while). Lorsque le compteur atteint 12, cela 
		veut dire qu'il n'y a pas eu de changements durant ces deux boucles for, et donc
		que l'on peut sortir de la boucle while. 
		*)


(* 3 cas :
 - les deux cases comparées sont toutes les 2 vides -> rien
 - la première case contient un nombre et la deuxième ne contient
 rien, 2 cas : 
 							-> déplacement de l'une des cases
 							mais le score reste inchangé
 							-> rien
- les deux cases contiennent un nombre, 2 cas :
							- si même nombre -> la case se déplace, le score
							change.
							- si pas le même nombre -> rien

*)

type crash = Collision | Deplacement | PasDeplacement;;

let deplacement a b dir = match a, b with 
		| 0, 0 -> PasDeplacement
		| c, d when c = d -> Collision
		|	0, d when dir = Gauche -> Deplacement
		| c, 0 when dir = Droite -> Deplacement
		| c, 0 when dir = Bas -> Deplacement
		| 0, d when dir = Haut -> Deplacement
		| _, _ -> PasDeplacement;;

(* PB : une case ne peut subir qu'une seule collision *)


let haut array score = 
		let compteur = ref 0 in
		while !compteur <> 12 do
			begin
				compteur := 0;
				for j = 1 to 3 do
						for i = 0 to 3 do
								if (deplacement array.(i).(j-1) array.(i).(j) Haut) = Collision
								then 
									begin
										array.(i).(j-1) <- array.(i).(j)+array.(i).(j-1);
										array.(i).(j) <- 0;
										score := !score + array.(i).(j-1);
										set_color rouge_fonce;
										fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
										affichage array score	
									end
								else 
									if (deplacement array.(i).(j-1) array.(i).(j) Haut) = Deplacement
									then 
										begin
											array.(i).(j-1) <- (array.(i).(j)+array.(i).(j-1));
											array.(i).(j) <- 0;
											set_color rouge_fonce;
											fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
											affichage array score	
										end
									else incr compteur;
						done
				done
			end
		done;;
		
let bas array score = 
		let compteur = ref 0 in 
		while !compteur <> 12	do 
			begin
				compteur := 0;
				for j = 2 downto 0 do 
						for i = 0 to 3 do
								if (deplacement array.(i).(j) array.(i).(j+1) Bas) = Collision
								then 
									begin
										array.(i).(j+1) <- array.(i).(j)+array.(i).(j+1);
										array.(i).(j) <- 0;
										set_color rouge_fonce;
										fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
										affichage array score								
									end
								else 
									if (deplacement array.(i).(j) array.(i).(j+1) Bas) = Deplacement
									then 
										begin
											array.(i).(j+1) <- (array.(i).(j)+array.(i).(j+1));
											array.(i).(j) <- 0;
											set_color rouge_fonce;
											fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
											affichage array score	
										end
									else incr compteur
						done
				done
			end
		done;;

let droite array score = 
		let compteur = ref 0 in
		while !compteur <> 12 do 
			begin
				compteur := 0;
				for i = 2 downto 0 do
						for j = 0 to 3 do
								if (deplacement array.(i).(j) array.(i+1).(j) Droite) = Collision 
								then
									begin
										array.(i+1).(j) <- array.(i).(j) + array.(i+1).(j);
										array.(i).(j) <- 0;
										set_color rouge_fonce;
										fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
										affichage array score
									end
								else 
									if (deplacement array.(i).(j) array.(i+1).(j) Droite) = Deplacement
									then
										begin
											array.(i+1).(j) <- (array.(i).(j)+array.(i+1).(j));
											array.(i).(j) <- 0;
											set_color rouge_fonce;
											fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
											affichage array score	
										end
									else incr compteur
						done
				done
			end
		done;;
		
let gauche array score = 
		let compteur = ref 0 in
		while !compteur <> 12 do
			begin
				compteur := 0;

				for i = 1 to 3 do
						for j = 0 to 3 do
								if (deplacement array.(i-1).(j) array.(i).(j) Gauche) = Collision
								then
									begin 
										array.(i-1).(j) <- array.(i).(j)+array.(i-1).(j);
										array.(i).(j) <- 0;
										set_color rouge_fonce;
										fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
										affichage array score		
									end
								else 
									if (deplacement array.(i-1).(j) array.(i).(j) Gauche) = Deplacement
									then 
										begin
											array.(i-1).(j) <- (array.(i).(j)+array.(i-1).(j));
											array.(i).(j) <- 0;
											set_color rouge_fonce;
											fill_rect (200 + i*150 +10) (450 - j*150 +10) 130 130;
											affichage array score	
										end
									else incr compteur
						done
				done
			end
		done;;



		(* appelle les 4 fonctions de déplacements précédentes 
		pour faire déplacer les cases *)
let output_direction dir array score = match dir with 
		|	Haut -> haut array score; new_tile array
		| Bas -> bas array score; new_tile array
		| Gauche -> gauche array score; new_tile array
		| Droite -> droite array score; new_tile array
		| Rien -> ();;
					

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

let play_again score = 
		set_color vert;
		fill_rect 330 50 300 100;
		moveto 370 90;
		set_text_size 30;
		set_color white;
		draw_string "PLAY AGAIN";
		if detecte_play_again 330 50 300 100 (wait_next_event Poll)
		then initialisation score else ();;

(* ------------------ ZONE DE TEST ------------------*)



(* ------------------ ZONE DE TEST -------------------*)


(* ---------- Fonction main ---------- *)

let g = let continuer = ref true in
				let tableau = make_matrix 4 4 0 in
				let	score = ref 0 in
				let toucheAppuyee = ref false in
				let touche = ref ' ' in
						open_graph ":0";
						resize_window 800 600;
						initialisation score;
						debut tableau;
						affichage tableau score;	

						while !continuer do
								toucheAppuyee := key_pressed();
								if !toucheAppuyee 
									then 
										begin
											touche := read_key();
											
											output_direction (direction_of_char !touche) tableau score;
											
											affichage tableau score;
											
											touche := ' ';
											toucheAppuyee := false;
										end;
								if array_rempli tableau 
										then if bloque tableau
											then 
											begin 
												print_game_over 0;
												continuer := false;
											end;
						done;;	