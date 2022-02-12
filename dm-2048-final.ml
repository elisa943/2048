(* 

Lorsque la fonction main est lancée, le jeu semble
bien fonctionner mais s'arrête à certains moments avec 
l'erreur suivante : "font set creation failure".

J'ai essayé de la résoudre mais je n'ai pas réussi.

Outre cela, j'ai normalement respecté toutes les règles du jeu c'est-à-dire :
 - déplacement de toutes les cases de la grille dans la direction associée
 - collision des cases de même chiffre qui renvoie une seule même case
 - les cases résultantes d'une collision ne peuvent pas être elles-mêmes en 
 collision avec une autre case dans un même tour. 
 - à chaque déplacement de la grille, une nouvelle case apparaît (sinon, 
 la grille reste immobile). 
 - lorsque la grille est rempli et que plus aucun mouvement n'est possible, 
 il y a game over. 
 
 
Commandes clavier : 

		Z ou z -> haut
		Q ou q -> gauche 
		S ou s -> bas
		D ou d -> droite

*)


#load "graphics.cma";;
open Graphics;;
open Random;;
open Array;;
open Char;;
exception GrilleBloquee;;
exception Stop;;



(* 
Sources 

https://htmlcolors.com/google-color-picker
https://caml.inria.fr/pub/docs/manual-caml-light/node16.html
https://musicordes.fr/tableau-frequences-notes/

*)

(* ---------- Variables préliminaires ----------  *)
open_graph ":0";;
resize_window 800 600;;
	
let bleu = rgb 38 92 153;;
let rouge = rgb 181 13 16;;
let vert = rgb 27 133 3;;
let rouge_fonce = rgb 204 45 48;;
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

let initialisation () = 
	begin
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
		end;
	done;
	end;;

(* ---------- Fonctions qui lit le tableau, l'affiche ---------- *)
	
		(* déplace le curseur veres la position du nombre (selon la taille du nombre) *)
let taille_nombre nb x y = match nb with
		| n when n < 10 -> moveto (x+60) (y+50)
		| n when n < 100 -> moveto (x+45) (y+50)
		| n when n < 1000 -> moveto (x+25) (y+50)
		| _ -> begin set_text_size 40; moveto (x+23) (y+55) end;;


let affiche_case i j nombre = 
	begin
		(* trace la case *)
		set_color (couleur_case nombre);
		fill_rect (200 + i*150 + 10) (450 - j*150 + 10) 130 130;
		(* trace le nombre *)
		set_color white;
		set_text_size 50;
		taille_nombre nombre (200 + i*150) (450 - j*150);
		draw_string (string_of_int nombre)
	end;;

let supprime_case i j = 
	begin
		set_color rouge_fonce;
		fill_rect (200 + i*150 + 10) (450 - j*150 + 10) 130 130
	end;;

		(* fonction qui affiche le score *)
let affiche_score sc = 
	begin
    set_color rouge_fonce;
		fill_rect 35 250 100 40;
		set_color white;
		set_text_size 25;
		moveto 35 250;
		draw_string (string_of_int sc)
	end;;
		
		(* fonction nécessaire pour être certain d'avoir des coordonnées
différentes c'est-à-dire ne pas avoir seulement une case qui 
apparaisse *)
let verification a b c d = if (a = c && b = d) then false else true;;

		(* fonction de départ : ajoute 2 cases au hasard dans la grille *)
let debut grille = let continuer = ref true in
		while !continuer do
				let a = int 4 and
				b = int 4 and
				c = int 4 and
				d = int 4 in
				if verification a b c d then
					begin
					grille.(a).(b) <- int_of_float (2.**float_of_int (int 2 + 1)); (* associe 2 ou 4 *)
					grille.(c).(d) <- int_of_float (2.**float_of_int (int 2 + 1));
					affiche_case a b grille.(a).(b);
					affiche_case c d grille.(c).(d);
					continuer := false
					end
				else ()
		done;;
		
(* ---------- Fonction qui choisit un emplacement libre random et ajoute 2 ou 4 ---------- *)

let nouvelle_case grille = 
	try
		while true do
				let a = int 4 and b = int 4 in
				if grille.(a).(b) = 0 
						then 
							begin
							grille.(a).(b) <- int_of_float (2.**float_of_int (int 2 + 1)); (* génère 2 ou 4 *)
							affiche_case a b grille.(a).(b);
							raise Stop
							end
		done;
	with Stop -> ();;

(* ---------- Fonctions qui détecte le game over ---------- *)

		(* fonction auxiliaire utilisée dans la fonction suivante *)
let condition_vide element = element > 0;;

		(* fonction qui renvoie true si la grille est remplie *)
let grille_rempli grille = 
		for_all condition_vide grille.(0) && 
		for_all condition_vide grille.(1) &&
		for_all condition_vide grille.(2) &&
		for_all condition_vide grille.(3);;
		
		(* fonction auxiliaire qui renvoie true s'il existe 
		deux mêmes éléments d'affilée *)
let deux_daffilee liste = 
		let tmp = ref liste.(0) and
		boo = ref false in
		for i = 1 to 3 do
				if !tmp = liste.(i) then boo := true 
					else tmp := liste.(i)
		done;
		!boo;;


		(* fonction qui échange lignes et colonnes *)
let transpose grille = 
		let grille_bis = make_matrix 4 4 0 in
				for i = 0 to 3 do 
						for j = 0 to 3 do 
								grille_bis.(i).(j) <- grille.(j).(i)
						done;
				done;
		grille_bis;;		

(* fonctions qui vérifient s'il y a 2 cases d'affilées de même nature 
	 dans chaque ligne/colonne *)

let lignes_bloquees grille = 
	try
		for i = 0 to 3 do 
				if deux_daffilee grille.(i) = false 
					then raise GrilleBloquee
		done;
		false;
	with GrilleBloquee -> true;;
		
let colonnes_bloquees grille = 
	try 
		let new_grille = transpose grille in
		for i = 0 to 3 do 
				if deux_daffilee new_grille.(i)
					then raise GrilleBloquee;
		done;
		false;
	with GrilleBloquee -> true;;
	
				
(* ---------- Fonctions qui détecte le déplacement du joueur---------- *)

type crash = Collision | Deplacement | PasDeplacement;;

		(* cette fonction traite tous les cas possibles de configuration des cases.
		
		Lorsque le potentiel déplacement d'une case se trouve de façon verticale, 
		'a' désigne toujours la case au dessus de 'b'. *)
let deplacement a b dir = match a, b with 
		| 0, 0 -> PasDeplacement
		| c, d when c = d -> Collision
		|	0, d when dir = Gauche -> Deplacement
		| c, 0 when dir = Droite -> Deplacement
		| c, 0 when dir = Bas -> Deplacement
		| 0, d when dir = Haut -> Deplacement
		| _, _ -> PasDeplacement;;


		(* Les 4 fonctions suivantes fonctionnent de manière analogue :
		- But de la variable 'check' : vérifier s'il y a mouvement (sinon, on n'affiche pas de nouvelle case)
		- On crée une matrice 4x4 qui servira à déterminer les cases ayant subies une collision. 
		En effet, chaque case ne peut subir qu'une unique collision par tour du joueur.
		Cette nouvelle matrice indiquera 1 lorsque la case a subit une collision 
		et 0 sinon. Elle se réinitialise à chaque tour de boucle de 'while'. 
		- On définit une variable 'compteur' qui augmente s'il ne détecte aucun changement. 
		- Selon la direction du joueur, les boucles for ne commencent pas aux mêmes indices. 
		En effet, si la direction choisie est la droite, la dernière colonne ne sera pas
		impactée. 
		- Les différents cas : Collision, Déplacement, PasDeplacement. 
		Collision et Déplacement fonctionnent sensiblement de la même manière. 
		PasDeplacement incremente le compteur.
		- Lorsque le compteur atteint 12, cela veut dire qu'il n'y a pas eu de 
		changements durant ces deux boucles for, et donc que l'on peut sortir 
		de la boucle while. *);;


let haut grille score = let check = ref false in
	try
		let grille_collision = make_matrix 4 4 0 in
		while true do
			begin
				let compteur = ref 0 in
					for j = 1 to 3 do
							for i = 0 to 3 do
									let dir = deplacement grille.(i).(j-1) grille.(i).(j) Haut in 
									match dir with 
									| Collision ->
										if grille_collision.(i).(j) = 0
										then
										begin 
											grille.(i).(j-1) <- grille.(i).(j) + grille.(i).(j-1);
											grille.(i).(j) <- 0;
											affiche_case i (j-1) grille.(i).(j-1);
											supprime_case i j;
											grille_collision.(i).(j-1) <- 1;
											score := !score + grille.(i).(j-1);
											affiche_score !score
										end
										else incr compteur 
									| Deplacement ->
										begin
											grille.(i).(j-1) <- grille.(i).(j);
											grille.(i).(j) <- 0;
											affiche_case i (j-1) grille.(i).(j-1);
											supprime_case i j;
											grille_collision.(i).(j-1) <- grille_collision.(i).(j);
											grille_collision.(i).(j) <- 0 
										end
									| PasDeplacement -> incr compteur;
							done
					done;
					if !compteur < 12 then check := true;
					if !compteur = 12 then raise Stop;
			end
		done;
		!check;
	with Stop -> (!check);;


let bas grille score = let check = ref false in
	try 
		let grille_collision = make_matrix 4 4 0 in
		while true do
			begin
				let compteur = ref 0 in
					for j = 2 downto 0 do
							for i = 0 to 3 do
								let dir = deplacement grille.(i).(j) grille.(i).(j+1) Bas in match dir with
									| Collision ->
										if grille_collision.(i).(j) = 0 then
										begin
											grille.(i).(j+1) <- grille.(i).(j) + grille.(i).(j+1);
											grille.(i).(j) <- 0;
											affiche_case i (j+1) grille.(i).(j+1);
											supprime_case i j;
											grille_collision.(i).(j+1) <- 1;
											score := !score + grille.(i).(j+1);
											affiche_score !score
											end
										else incr compteur
									| Deplacement ->
										begin
											grille.(i).(j+1) <- grille.(i).(j);
											grille.(i).(j) <- 0;
											affiche_case i (j+1) grille.(i).(j+1);
											grille_collision.(i).(j+1) <- grille_collision.(i).(j);
											grille_collision.(i).(j) <- 0;
											supprime_case i j
										end
									| PasDeplacement -> incr compteur;
							done;
					done;
					if !compteur < 12 then check := true;
					if !compteur = 12 then raise Stop;
			end;
		done;
		!check
	with Stop -> !check;;


let droite grille score = let check = ref false in
	try
		let grille_collision = make_matrix 4 4 0 in
		while true do
			begin
				let compteur = ref 0 in
					for i = 2 downto 0 do
							for j = 0 to 3 do
									let dir = deplacement grille.(i).(j) grille.(i+1).(j) Droite in match dir with
										| Collision ->
										if grille_collision.(i).(j) = 0 then
											begin
												grille.(i+1).(j) <- grille.(i).(j) + grille.(i+1).(j);
												grille.(i).(j) <- 0;
												affiche_case (i+1) j grille.(i+1).(j);
												supprime_case i j;
												grille_collision.(i+1).(j) <- 1;
												score := !score + grille.(i+1).(j);
												affiche_score !score
											end
										else incr compteur
										| Deplacement ->
										begin
											grille.(i+1).(j) <- grille.(i).(j);
											grille.(i).(j) <- 0;
											affiche_case (i+1) j grille.(i+1).(j);
											supprime_case i j;
											grille_collision.(i+1).(j) <- grille_collision.(i).(j);
											grille_collision.(i).(j) <- 0 
										end
										| PasDeplacement -> incr compteur;
							done
					done;
					if !compteur < 12 then check := true;
					if !compteur = 12 then raise Stop;
			end
		done;
		!check;
	with Stop -> !check;;
		
let gauche grille score = let check = ref false in
	try
		let grille_collision = make_matrix 4 4 0 in
		while true do
			begin 
				let compteur = ref 0 in
					for i = 1 to 3 do
							for j = 0 to 3 do
									let dir = deplacement grille.(i-1).(j) grille.(i).(j) Gauche in match dir with
									| Collision ->
											if grille_collision.(i).(j) = 0 
												then
												begin
												grille.(i-1).(j) <- grille.(i).(j) + grille.(i-1).(j);
												grille.(i).(j) <- 0;
												affiche_case (i-1) j grille.(i-1).(j);
												supprime_case i j;
												grille_collision.(i-1).(j) <- 1;
												score := !score + grille.(i-1).(j);
												affiche_score !score
												end
										else incr compteur
									| Deplacement ->
												begin
												grille.(i-1).(j) <- grille.(i).(j);
												grille.(i).(j) <- 0;
												affiche_case (i-1) j grille.(i-1).(j);
												supprime_case i j;
												grille_collision.(i-1).(j) <- grille_collision.(i).(j);
												grille_collision.(i).(j) <- 0 
												end
									| PasDeplacement -> incr compteur
							done
					done;
					if !compteur < 12 then check := true;
					if !compteur = 12 then raise Stop
			end
		done;
		!check;
	with Stop -> !check;;




		(* appelle les 4 fonctions de déplacement précédentes 
		puis ajoute une une nouvelle case. *)
let output_direction dir grille score = match dir with 
		|	Haut -> if haut grille score then nouvelle_case grille else ()
		| Bas -> if bas grille score then nouvelle_case grille else ()
		| Gauche -> if gauche grille score then nouvelle_case grille else ()
		| Droite -> if droite grille score then nouvelle_case grille else ()
		| Rien -> ();;

(* ---------- Fonction qui affiche l'écran de game over ---------- *)

let print_game_over () = 
	begin
    set_color vert;
    fill_rect 300 200 360 200;
		moveto 330 280;
    set_text_size 50;
    set_color white;
    draw_string "GAME OVER"
   end;;


(* ---------- Fonction main ---------- *)

let main = 
		try
				let grille = make_matrix 4 4 0 and
				score = ref 0 and
				toucheAppuyee = ref false and
				touche = ref ' ' in
					begin
						open_graph ":0";
						resize_window 800 600;
						initialisation ();
						debut grille;
						affiche_score !score;	
						while true do
								toucheAppuyee := key_pressed();
								if !toucheAppuyee 
									then
										begin
											touche := read_key(); 
											output_direction (direction_of_char !touche) grille score;
											toucheAppuyee := false;
										end
									else 
										if grille_rempli grille 
										then if lignes_bloquees grille && colonnes_bloquees grille
														then 
															begin 
															print_game_over ();
															raise Stop
															end
											
						done;
					end
			with Stop -> () ;;