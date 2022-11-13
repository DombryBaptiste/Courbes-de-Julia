#use "topfind";;
#require "graphics";;
open Graphics;;
open Complex;;
open Scanf;;

type command =
	| NewComplex of t
	| ResizeWindow of int
	| Zoom of float
	| PointCentral of t
	| Borne of int;;

type config = { complexe_c : t;
							 dim_fenetre : int;
							 zoom : float;
							 point_central : t;
							 borne : int };;

let config = { complexe_c = { re = 0.285; im = 0.01;};
							dim_fenetre = 600;
							zoom = 4.;
							point_central = { re = -2.; im = -2.;};
							borne = 256;};;

let lvl_of c conf =
	let rec aux n acc =
		if(n >= conf.borne) || (norm acc > 2.) then n
		else aux (n +1) (add(mul acc acc) conf.complexe_c)
	in aux 0 c;;

let julia conf =
	let calcul_complex x y =
		add conf.point_central 
			{ re = (float_of_int x) *. conf.zoom /. float_of_int conf.dim_fenetre ;
			 im = (float_of_int y) *. conf.zoom /. float_of_int conf.dim_fenetre ;} in
	let calcul_couleur n = rgb n n n; in
	let rec colonne y =
		let rec ligne x =
			let calc = calcul_complex x y in
			let n = lvl_of calc conf in
			set_color (calcul_couleur n);
			plot x y;
			if x < conf.dim_fenetre then ligne (x +1) in ligne 0;
		if y < conf.dim_fenetre then colonne (y +1) in colonne 0;;

	
let read_command() =
	let read = read_line() in
		let scan = sscanf read "%s %s %s" (fun a b c ->  (a,b,c)) in match scan with
			| ("NewComplex", a, b) -> NewComplex  {re = (sscanf a "%f" (fun x -> x)); im = (sscanf b "%f" (fun x -> x))}
			| ("ResizeWindow", a, _) -> ResizeWindow  (sscanf a "%d" (fun x -> x))
			| ("Zoom", a, _) -> Zoom  (sscanf a "%f" (fun x -> x))
			| ("PointCentral", a, b) -> PointCentral  {re = (sscanf a "%f" (fun x -> x)); im = (sscanf b "%f" (fun x -> x))}
			| ("Borne", a, _) -> Borne  (sscanf a "%d" (fun x -> x))
			| _ -> failwith "erreur";;
    
let execute a = 
	let return_read = read_command() in match return_read with
	| NewComplex x  -> {complexe_c = x ; dim_fenetre = a.dim_fenetre ; zoom = a.zoom ; point_central = a.point_central ; borne = a.borne;}
	| ResizeWindow x -> {complexe_c = a.complexe_c ; dim_fenetre = x ; zoom = a.zoom; point_central = a.point_central ; borne = a.borne;}
	| Zoom x -> {complexe_c = a.complexe_c ; dim_fenetre = a.dim_fenetre ; zoom = x; point_central = a.point_central ; borne = a.borne;}
	| PointCentral x -> {complexe_c = a.complexe_c ; dim_fenetre = a.dim_fenetre ; zoom = a.zoom; point_central = x ; borne = a.borne;}
	| Borne x -> {complexe_c = a.complexe_c ; dim_fenetre = a.dim_fenetre ; zoom = a.zoom; point_central = a.point_central ; borne = x;};;
	
let rec read_command_and_draw conf =
	let new_conf = execute conf in 
	if conf.dim_fenetre != new_conf.dim_fenetre then resize_window (new_conf.dim_fenetre) (new_conf.dim_fenetre) ;
		julia new_conf;
		read_command_and_draw new_conf;;

let main conf =
	let a = " "^string_of_int(conf.dim_fenetre)^ "x" ^string_of_int(conf.dim_fenetre) in
	open_graph a;
	print_string "Entrez une commande\n - NewComplex float float\n - ResizeWindow int\n - Zoom float\n - PointCentral float float\n - Borne int\n";
	
	julia conf;
	read_command_and_draw conf;;

main config;;







