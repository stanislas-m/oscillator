(**
* Mecanic or electromagnetic oscillator
*
* @author Alan BARD, Céline LEPICARD and Stanislas MICHALAK
* Require liblablgtk2-ocaml-dev
* To compile: ocaml -w s -I +lablgtk2 lablgtk.cma main.ml
*)

(* Initialisation de GTK. *)
let _ = GMain.init ()

(**
* Fenêtre de confirmation pour quitter le programme
* @param window GWindow.window La fenêtre principale de l'application
*)
let quit window =
	let dlg =
		GWindow.message_dialog
		~message:"Voulez-vous vraiment quitter le programme ?"
		~parent:window
		~destroy_with_parent:true
		~message_type:`QUESTION
		~buttons:GWindow.Buttons.yes_no () in
			if dlg#run () = `NO then
		  		dlg#destroy ()
			else
				GMain.quit ()
;;

(******************************************************************************
*                          Validation des données 
******************************************************************************)

type validField = Valid | Invalid;;

(**
* Traitement du formulaire de configuration d'un oscillateur mécanique
* @param k string
* @param m string
* @param x0 string
* @param v0 string
* @param forcedMode int
* @param pulse string
* @param amp string
*)
let process_meca_config k m lambda x0 v0 forcedMode pulse amp =
   [|(try
      if float_of_string k < 0. then failwith "float_of_string" else Valid
   with Failure "float_of_string" -> Invalid);
   (try
      if float_of_string m < 0. then failwith "float_of_string" else Valid
   with Failure "float_of_string" -> Invalid);
   (try
      if float_of_string lambda < 0. then failwith "float_of_string" else Valid
   with Failure "float_of_string" -> Invalid);
   (try
      if float_of_string x0 < 0. then failwith "float_of_string" else Valid
   with Failure "float_of_string" -> Invalid);
   (try
      if float_of_string v0 < 0. then failwith "float_of_string" else Valid
   with Failure "float_of_string" -> Invalid);
   (if forcedMode = 1 then
      (try
         if float_of_string pulse < 0. then failwith "float_of_string" else Valid
      with Failure "float_of_string" -> Invalid)
   else
      Valid);
   (if forcedMode = 1 then
      (try
         if float_of_string amp < 0. then failwith "float_of_string" else Valid
      with Failure "float_of_string" -> Invalid)
   else
      Valid)
   |]
;;

let process_elec_config r l c q0 i0 forcedMode pulse amp =
   [|(try
		if float_of_string r < 0. then failwith "float_of_string" else Valid
	with Failure "float_of_string" -> Invalid); 
	(try
		if float_of_string l < 0. then failwith "float_of_string" else Valid
	with Failure "float_of_string" -> Invalid);
	(try
		if float_of_string c < 0. then failwith "float_of_string" else Valid
	with Failure "float_of_string" -> Invalid);
	(try
		if float_of_string q0 < 0. then failwith "float_of_string" else Valid
	with Failure "float_of_string" -> Invalid);
	(try
		if float_of_string i0 < 0. then failwith "float_of_string" else Valid
	with Failure "float_of_string" -> Invalid);
	(if forcedMode = 1 then
		(try
         if float_of_string pulse < 0. then failwith "float_of_string" else Valid
      with Failure "float_of_string" -> Invalid)
	else
		Valid);
	(if forcedMode = 1 then
      (try
         if float_of_string amp < 0. then failwith "float_of_string" else Valid
      with Failure "float_of_string" -> Invalid)
   else
      Valid)
	|]
;;

(******************************************************************************
*                          Affichage des résultats 
******************************************************************************)

(************************* Gestion des graphiques ****************************)

(**
* Callback pour l'évènement expose (gère la réécriture du graphe lorsque
* la fenêtre a été masquée ou recouverte
*
* @param drawingArea GMisc.drawing_area La zone de dessin visible
* @param backing GDraw.pixmap La zone de dessin en mémoire
*)
let exposeEvent (drawingArea:GMisc.drawing_area) (backing:GDraw.pixmap) ev =
  let area = GdkEvent.Expose.area ev in
    let x = Gdk.Rectangle.x area in
	   let y = Gdk.Rectangle.y area in
		  let width = Gdk.Rectangle.width area in
		    let height = Gdk.Rectangle.height area in
			   let drawing =
				    drawingArea#misc#realize ();
					     new GDraw.drawable (drawingArea#misc#window)
						    in
							   drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height backing#pixmap;
								  false
;;

(**
* Récupère le xMax parmi un ensemble de courbes données
* @param curves (float * float) list list La liste de courbes
*)
let get_xMax curves =
	let compare x y =
	if x < y then 1
		else if x > y then -1
		else 0 in
   if curves <> [] then
	   List.hd (List.fast_sort compare (List.map (fun curve -> match (List.split curve) with
	(x,_) -> (List.hd (List.fast_sort compare x))) curves))
	else
	   0.
;;

(**
* Récupère le yMax parmi un ensemble de courbes données
* @param curves (float * float) list list La liste de courbes
*)
let get_yMax curves =
	let compare x y =
	if x < y then 1
		else if x > y then -1
		else 0 in
	if curves <> [] then
	   List.hd (List.fast_sort compare (List.map abs_float (List.map (fun curve -> match (List.split curve) with
	(_,y) -> (List.hd (List.fast_sort compare y))) curves)))
	else
	   0.
;;

(**
* Dessine les courbes renseignées par le paramètre « curves »
* @param backing GDraw.pixmap La zone de dessin en mémoire
* @param curves (float * float) list list La liste de courbes
*)
let rec draw_curves (backing:GDraw.pixmap) curves xScale yScale =
	let height = snd backing#size in
	if List.length curves > 0 then
	begin
	   backing#set_line_attributes ~width:2 ();
		backing#lines (List.map (fun (x,y) -> ((int_of_float (x *. xScale +. 15.)), int_of_float ((float_of_int (height/2)) -. y *. yScale)))  (List.hd curves));
		draw_curves backing (List.tl curves) xScale yScale;
		backing#set_line_attributes ~width:1 ();
		backing
	end
	else
		backing		
;;

let rec draw_axes_segments_x (backing:GDraw.pixmap) x height width  =
   if ((x - 15) mod 50 = 0) then
      backing#line x ((height / 2) - 3) x ((height / 2) + 3)
   else
      backing#line x ((height / 2) - 3) x (height / 2);
   if x <= (width - 15) then
      draw_axes_segments_x backing (x + 10) height width
   else
      backing
;;

let draw_axes (backing:GDraw.pixmap) yScale =
	let height = snd backing#size in
	let width = fst backing#size in
	backing#set_foreground `WHITE;
	backing#line 15 2 15 height;
	backing#polygon ~filled:true [(8,10);(15,0);(22,10)];
	backing#polygon ~filled:true [(width-10,(height/2)-7);(width,height/2);(width-10,(height/2)+7)];
	backing#line 0 (height/2) (width-2) (height/2);
	draw_axes_segments_x backing 15 height width;
;;

let draw_graph contener curves =
   let xMax = get_xMax curves and yMax = get_yMax curves in
	let height = 452 and width = (if ((int_of_float xMax) * 10 < 602) then 602 else (int_of_float xMax) * 10) in
	let xScale = 10. and yScale = ((float_of_int height /. 2.) -. 15.) /. yMax in
	let drawingArea = GMisc.drawing_area ~width ~height ~packing:contener#add () in
 		let backing = GDraw.pixmap ~width ~height () in
			let drawing = drawingArea#misc#realize (); new GDraw.drawable drawingArea#misc#window in
   		drawingArea#misc#modify_bg [(`NORMAL,`WHITE)];
			drawingArea#event#connect#expose ~callback:(fun event -> exposeEvent drawingArea backing event);
			backing#set_foreground `BLACK;
			backing#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
			draw_axes backing yScale;
			draw_curves backing curves xScale yScale;
			drawing#put_pixmap ~x:0 ~y:0 backing#pixmap;
	()
;;

(****************** Récupération et affichage des résultats ******************)

(**
* Génère une liste de points à partir d'une fonction donnée
* @param f (float -> float) La fonction génératrice
* @param x float La valeur en abscisse actuelle
* @param currentZeros int Le nombre de fois consécutives où l'on est entré dans l'intervalle -0,01 < y < 0,01
* @param lambda float Le coefficient d'amortissement
*)
let rec get_points_from_function f x currentZeros lambda =
	if currentZeros >= 5 then
		[]
	else
	   ((x,f x)::(get_points_from_function f (x +. (if lambda = 0. then 0.005 else 0.2)) (if f x < 0.01 && f x > -.0.01 then currentZeros + 1 else if lambda = 0. then currentZeros else 0) lambda))	   
;;

let truncate x n =
   (float_of_int (int_of_float (x *. 10. ** (float_of_int n)))) /. (10. ** (float_of_int n)) 
;;

let rec display_meca_results window k m lambda x0 v0 forcedMode pulse amp =
   let resultsWindow = GWindow.dialog ~title:"Résultats pour l'oscillateur mécanique demandé" ~height:560 ~width:900 ~modal:true ~destroy_with_parent:true ~parent:window () in
      GMisc.label ~markup:"<span font_size='xx-large'><b>Résultats pour l'oscillateur mécanique demandé</b></span>" ~packing:resultsWindow#vbox#add ();
		let w0 = sqrt((float_of_string k) /. (float_of_string m)) and xi = (float_of_string lambda) /. (2. *. sqrt((float_of_string k) *. (float_of_string m))) in
		let x t =
			if (float_of_string lambda) = 0. then
				(float_of_string x0) *. (cos (w0 *. t)) +. ((float_of_string v0) /. w0) *. (sin (w0 *. t))
			 else if xi < 1. then
			   let w = w0 *. sqrt (1.-. xi**2.) in
			   (exp (-.xi *. w0 *. t)) *. ((((float_of_string v0) +. xi *. w0 *. (float_of_string x0)) /. w) *. (sin (w *. t)) +. (float_of_string x0) *. (cos (w *. t))) 
			 else
				(exp (-.xi *. w0 *. t)) *. ((float_of_string x0) +. ((float_of_string v0) +. w0 *. (float_of_string x0)) *. t)
		in
		let resultsBox = GPack.hbox ~spacing:10 ~packing:resultsWindow#vbox#add () in
         let graphFrame = GBin.frame ~label:"Graphe" ~packing:resultsBox#add () in
            let graphFrameScroll = GBin.scrolled_window ~vpolicy:`NEVER ~hpolicy:`AUTOMATIC ~height:452 ~width:602 ~packing:graphFrame#add () in
               let graphFrameContent = GBin.viewport ~packing:graphFrameScroll#add () in
               draw_graph graphFrameContent [(get_points_from_function x 0. 0 (float_of_string lambda))];
         let configResultsBox = GPack.vbox ~spacing:10 ~width:280 ~packing:resultsBox#add () in
				let configFrame = GBin.frame ~label:"Configuration" ~packing:configResultsBox#add () in
					let configFrameContent = GPack.vbox ~spacing:10 ~packing:configFrame#add () in
						let configOwnInit = GPack.hbox ~spacing:10 ~packing:configFrameContent#add () in
							let configOwn = GPack.vbox ~packing:configOwnInit#add () in
								GMisc.label ~markup:("<b>k</b> : " ^ k) ~packing:configOwn#add ();
								GMisc.label ~markup:("<b>m</b> : " ^ m ^ " g") ~packing:configOwn#add ();
								GMisc.label ~markup:("<b>λ</b> : " ^ lambda) ~packing:configOwn#add ();
							let configInit = GPack.vbox ~packing:configOwnInit#add () in
								GMisc.label ~markup:("<b>x<sub>0</sub></b> : " ^ x0 ^ " cm") ~packing:configInit#add ();
								GMisc.label ~markup:("<b>v<sub>0</sub></b> : " ^ v0 ^ " cm.s<sup>-1</sup>") ~packing:configInit#add ();
				let resultsFrame = GBin.frame ~label:"Résultats" ~packing:configResultsBox#add () in
				   let resultsFrameContent = GPack.vbox ~spacing:10 ~packing:resultsFrame#add () in
				   GMisc.label ~markup:("<b>Facteur de qualité</b> : " ^ (string_of_float (truncate (w0 /. (2. *. (float_of_string lambda))) 3))) ~packing:resultsFrameContent#add ();
	            GMisc.label ~markup:("<b>Pulsation propre</b> : " ^ (string_of_float (truncate w0 3))) ~packing:resultsFrameContent#add ();
	            GMisc.label ~markup:("<b>Pulsation de résonance</b> : " ^ (if ((float_of_string lambda) < w0 /. (sqrt 2.)) then string_of_float(truncate (sqrt ((w0 ** 2.) -. (2. *. ((float_of_string lambda) ** 2.)))) 3) else "indéfinie")) ~packing:resultsFrameContent#add ();
      let backButtonBox = GPack.button_box `HORIZONTAL ~packing:resultsWindow#vbox#add () in
         let backToConfigButton = GButton.button ~label:"Changer la configuration" ~packing:backButtonBox#add () in
            backToConfigButton#connect#clicked ~callback:(fun () -> resultsWindow#destroy ();start_meca_config ~k:k ~m:m ~lambda:lambda ~x0:x0 ~v0:v0 window);
            GMisc.image ~stock:`EDIT ~packing:backToConfigButton#set_image ();
         let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:backButtonBox#add () in
            backToMenuButton#connect#clicked ~callback:(fun () -> resultsWindow#destroy ());
            GMisc.image ~stock:`GO_BACK ~packing:backToMenuButton#set_image ();
   resultsWindow#show ()

and display_elec_results window r l c q0 i0 forcedMode pulse amp =
 let resultsWindow = GWindow.dialog ~title:"Résultats pour l'oscillateur électrocinétique demandé" ~height:560 ~width:900 ~modal:true ~destroy_with_parent:true ~parent:window () in
      GMisc.label ~markup:"<span font_size='xx-large'><b>Résultats pour l'oscillateur électrocinétique demandé</b></span>" ~packing:resultsWindow#vbox#add ();
		let w0 = sqrt((1. /. float_of_string c) /. (float_of_string l)) and xi = (float_of_string r) /. (2. *. sqrt((1. /. (float_of_string c)) *. (float_of_string l))) in
		let q t =
			if (float_of_string r) = 0. then
				(float_of_string q0) *. (cos (w0 *. t)) +. ((float_of_string i0) /. w0) *. (sin (w0 *. t))
			 else if xi < 1. then
			   let w = w0 *. sqrt (1.-. xi**2.) in
			   (exp (-.xi *. w0 *. t)) *. ((((float_of_string i0) +. xi *. w0 *. (float_of_string q0)) /. w) *. (sin (w *. t)) +. (float_of_string q0) *. (cos (w *. t))) 
			 else
				(exp (-.xi *. w0 *. t)) *. ((float_of_string q0) +. ((float_of_string i0) +. w0 *. (float_of_string q0)) *. t)
		in
		let resultsBox = GPack.hbox ~spacing:10 ~packing:resultsWindow#vbox#add () in
         let graphFrame = GBin.frame ~label:"Graphe" ~packing:resultsBox#add () in
            let graphFrameScroll = GBin.scrolled_window ~vpolicy:`NEVER ~hpolicy:`AUTOMATIC ~height:452 ~width:602 ~packing:graphFrame#add () in
               let graphFrameContent = GBin.viewport ~packing:graphFrameScroll#add () in
               draw_graph graphFrameContent [(get_points_from_function q 0. 0 (float_of_string r))];
         let configResultsBox = GPack.vbox ~spacing:10 ~width:280 ~packing:resultsBox#add () in
				let configFrame = GBin.frame ~label:"Configuration" ~packing:configResultsBox#add () in
					let configFrameContent = GPack.vbox ~spacing:10 ~packing:configFrame#add () in
						let configOwnInit = GPack.hbox ~spacing:10 ~packing:configFrameContent#add () in
							let configOwn = GPack.vbox ~packing:configOwnInit#add () in
								GMisc.label ~markup:("<b>R</b> : " ^ r ^ " Ω") ~packing:configOwn#add ();
								GMisc.label ~markup:("<b>L</b> : " ^ l ^ " H") ~packing:configOwn#add ();
								GMisc.label ~markup:("<b>C</b> : " ^ c ^ " F") ~packing:configOwn#add ();
							let configInit = GPack.vbox ~packing:configOwnInit#add () in
								GMisc.label ~markup:("<b>q<sub>0</sub></b> : " ^ q0 ^ " C") ~packing:configInit#add ();
								GMisc.label ~markup:("<b>I<sub>0</sub></b> : " ^ i0 ^ " A") ~packing:configInit#add ();
				let resultsFrame = GBin.frame ~label:"Résultats" ~packing:configResultsBox#add () in
				   let resultsFrameContent = GPack.vbox ~spacing:10 ~packing:resultsFrame#add () in
				   GMisc.label ~markup:("<b>Facteur de qualité</b> : " ^ (string_of_float (truncate (w0 /. (2. *. (float_of_string r))) 3))) ~packing:resultsFrameContent#add ();
	            GMisc.label ~markup:("<b>Pulsation propre</b> : " ^ (string_of_float (truncate w0 3))) ~packing:resultsFrameContent#add ();
	            GMisc.label ~markup:("<b>Pulsation de résonance</b> : " ^ (if ((float_of_string r) < w0 /. (sqrt 2.)) then string_of_float(truncate (sqrt ((w0 ** 2.) -. (2. *. ((float_of_string r) ** 2.)))) 3) else "indéfinie")) ~packing:resultsFrameContent#add ();
      let backButtonBox = GPack.button_box `HORIZONTAL ~packing:resultsWindow#vbox#add () in
         let backToConfigButton = GButton.button ~label:"Changer la configuration" ~packing:backButtonBox#add () in
            backToConfigButton#connect#clicked ~callback:(fun () -> resultsWindow#destroy ();start_elec_config ~r:r ~l:l ~c:c ~q0:q0 ~i0:i0 window);
            GMisc.image ~stock:`EDIT ~packing:backToConfigButton#set_image ();
         let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:backButtonBox#add () in
            backToMenuButton#connect#clicked ~callback:(fun () -> resultsWindow#destroy ());
            GMisc.image ~stock:`GO_BACK ~packing:backToMenuButton#set_image ();
   resultsWindow#show ()

(******************************************************************************
*                          Fenêtres de configuration 
******************************************************************************)

and start_meca_config ?k:(k=("")) ?m:(m="") ?lambda:(lambda="") ?x0:(x0="") ?v0:(v0="") window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur mécanique" ~height:410 ~width:750 ~parent:window ~modal:true ~destroy_with_parent:true () in
	let messageLabel = GMisc.label ~packing:dialogBox#vbox#add () in
	let ownInitHBox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
  		let ownParamsFrame = GBin.frame ~label:"Paramètres propres" ~packing:ownInitHBox#add () in
			let frameContent = GPack.vbox ~spacing:10 ~packing:ownParamsFrame#add () in
				let kBox = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
					let kLabel = GMisc.label ~markup:"<b>k</b> : " ~packing:kBox#add () in
					let kInput = GEdit.entry ~text:k ~width:20 ~packing:kBox#add () in
					GMisc.label ~text: " " ~packing:kBox#add ();	
				let mBox = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
					let mLabel = GMisc.label ~markup:"<b>m</b> : " ~packing:mBox#add () in
					let mInput = GEdit.entry ~text:m ~width:20 ~packing:mBox#add () in
					GMisc.label ~text: "g" ~packing:mBox#add ();
				let lambdaBox = GPack.hbox~spacing:10 ~packing:frameContent#add () in
					let lambdaLabel = GMisc.label ~markup:"<b>λ</b> : " ~packing:lambdaBox#add () in
					let lambdaInput = GEdit.entry ~text:lambda ~width:20 ~packing:lambdaBox#add () in
					GMisc.label ~text: " " ~packing:lambdaBox#add ();
   		let initCondFrame = GBin.frame ~label:"Conditions initiales" ~packing:ownInitHBox#add () in
				let frameContent = GPack.vbox ~spacing:10 ~packing:initCondFrame#add () in
					let x0Box = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
						let x0Label = GMisc.label ~markup:"<b>x<sub>0</sub></b>" ~packing:x0Box#add () in
						let x0Input = GEdit.entry ~text:x0 ~width:20 ~packing:x0Box#add () in
						GMisc.label ~text:"cm" ~packing:x0Box#add ();
					let v0Box = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
           			let v0Label = GMisc.label ~markup:"<b>v<sub>0</sub></b>" ~packing:v0Box#add () in
            		let v0Input = GEdit.entry ~text:v0 ~width:20 ~packing:v0Box#add () in
            		GMisc.label ~markup:"cm.s<sup>-1</sup>" ~packing:v0Box#add ();	
 	let forcedRegimeTypeFrame = GBin.frame ~label:"Régime forcé" ~packing:dialogBox#vbox#add () in
     	let forcedRegimeTypeFrameContent = GPack.vbox ~spacing:10 ~packing:forcedRegimeTypeFrame#add () in
        	let regimeTypeBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
           	GMisc.label ~markup:"<b>Type</b> : " ~packing:regimeTypeBox#add (); 
           	let regimesList = GEdit.combo_box_text ~strings:["Constant";"Sinusoïdal"] ~active:0 ~packing:regimeTypeBox#add () in
              	let regimesListCBox = match regimesList with (x,y) -> x in
        	let amplBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
           	let amplLabel = GMisc.label ~markup:"<b>Amplitude</b> : " ~packing:amplBox#add () in
           	let amplInput = GEdit.entry ~editable:false ~packing:amplBox#add () in
        	let pulseBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
           	let pulseLabel = GMisc.label ~markup:"<b>Pulsation</b> : " ~packing:pulseBox#add () in
           	let pulseInput = GEdit.entry ~editable:false ~packing:pulseBox#add () in
        	ignore (regimesListCBox#connect#changed ~callback:(fun () -> if regimesListCBox#active = 0 then (amplInput#set_editable false;amplInput#set_text "";pulseInput#set_editable false;pulseInput#set_text "") else (amplInput#set_editable true;pulseInput#set_editable true)));
	let buttonBox = GPack.button_box `HORIZONTAL ~spacing:10 ~packing:dialogBox#vbox#add () in
   	let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:buttonBox#add () in
     		backToMenuButton#connect#clicked ~callback:(fun () -> dialogBox#destroy ());
     		GMisc.image ~stock:`GO_BACK ~packing:backToMenuButton#set_image (); 
     	let validateButton = GButton.button ~label:"Valider" ~packing:buttonBox#add () in
   		validateButton#connect#clicked ~callback:(
			fun () -> (
				(* Validation du formulaire *)
      		let validationResult = process_meca_config kInput#text mInput#text lambdaInput#text x0Input#text v0Input#text regimesListCBox#active pulseInput#text amplInput#text in
         		(* Affichage des éventuelles erreurs *)
         		if List.exists (fun x -> x = Invalid) (Array.to_list validationResult) then
         		begin
            		messageLabel#set_text "<span foreground='red'>La configuration saisie présente des erreurs</span>";
            		messageLabel#set_use_markup true;
						(if Array.get validationResult 0 = Invalid then
         			begin
            			kLabel#set_text "<span foreground='red'><b>k</b> : </span>";
            			kLabel#set_use_markup true
         			end
         			else
            			kLabel#set_text "<b>k</b> : ";kLabel#set_use_markup true);
         			(if Array.get validationResult 1 = Invalid then
         			begin
            			mLabel#set_text "<span foreground='red'><b>m</b> : </span>";
            			mLabel#set_use_markup true
        				end
         			else
            			mLabel#set_text "<b>m</b> : ";mLabel#set_use_markup true);
         			(if Array.get validationResult 2 = Invalid then
         			begin
            			lambdaLabel#set_text "<span foreground='red'><b>λ</b> : </span>";
            			lambdaLabel#set_use_markup true
         			end
         			else
            			lambdaLabel#set_text "<b>λ</b> : ";lambdaLabel#set_use_markup true);
         			(if Array.get validationResult 3 = Invalid then
         			begin
            			x0Label#set_text "<span foreground='red'><b>x<sub>0</sub></b> : </span>";
            			x0Label#set_use_markup true
         			end
         			else
            			x0Label#set_text "<b>x<sub>0</sub></b> : ";x0Label#set_use_markup true);
         			(if Array.get validationResult 4 = Invalid then
         			begin
            			v0Label#set_text "<span foreground='red'><b>v<sub>0</sub></b> : </span>";
            			v0Label#set_use_markup true
         			end
         			else
            			v0Label#set_text "<b>v<sub>0</sub></b> : ";v0Label#set_use_markup true);
         			(if Array.get validationResult 5 = Invalid then
         			begin
            			pulseLabel#set_text "<span foreground='red'><b>Pulsation</b> : </span>";
            			pulseLabel#set_use_markup true
         			end
         			else
            			pulseLabel#set_text "<b>Pulsation</b> : ";pulseLabel#set_use_markup true);
         			(if Array.get validationResult 6 = Invalid then
         			begin
            			amplLabel#set_text "<span foreground='red'><b>Amplitude</b> : </span>";
            			amplLabel#set_use_markup true
         			end
         			else
            			amplLabel#set_text "<b>Amplitude</b> : ";amplLabel#set_use_markup true)
					end
					else
					begin
						let k = kInput#text and m = mInput#text and lambda = lambdaInput#text and x0 = x0Input#text and v0 = v0Input#text and forcedMode = regimesListCBox#active and pulse = pulseInput#text and ampl = amplInput#text in
                  dialogBox#destroy ();
						display_meca_results window k m lambda x0 v0 forcedMode pulse ampl
					end
			));
			GMisc.image ~stock:`OK ~packing:validateButton#set_image ();
  dialogBox#show ()

and start_elec_config ?r:(r=("")) ?l:(l="") ?c:(c="") ?q0:(q0="") ?i0:(i0="") window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur électrocinétique" ~height:480 ~width:750 ~parent:window ~modal:true ~destroy_with_parent:true () in
  	let messageLabel = GMisc.label ~packing:dialogBox#vbox#add () in
	let ownInitHbox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
	let illustration = GMisc.image ~width:200 ~packing:ownInitHbox#add () in
		illustration#set_file "./img/oscillator_elec_config.png";
  	let ownParamsFrame = GBin.frame ~label:"Paramètres propres" ~packing:ownInitHbox#add () in
		let ownParamsFrameContent = GPack.vbox ~spacing:10 ~packing:ownParamsFrame#add () in
			let rBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let rLabel = GMisc.label ~markup:"<b>R</b> : " ~packing:rBox#add () in
				let rInput = GEdit.entry ~text:r ~width:20 ~packing:rBox#add () in
				GMisc.label ~text:"Ω" ~packing:rBox#add ();
			let lBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let lLabel = GMisc.label ~markup:"<b>L</b> : " ~packing:lBox#add () in
				let lInput = GEdit.entry ~text:l ~width:20 ~packing:lBox#add () in
				GMisc.label ~text:"H" ~packing:lBox#add ();
			let cBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let cLabel = GMisc.label ~markup:"<b>C</b> : " ~packing:cBox#add () in
				let cInput = GEdit.entry ~text:c ~width:20 ~packing:cBox#add () in
				GMisc.label ~text:"F" ~packing:cBox#add ();
	   let initCondFrame = GBin.frame ~label:"Conditions initiales" ~packing:ownInitHbox#add () in
				let frameContent = GPack.vbox ~spacing:10 ~packing:initCondFrame#add () in
					let q0Box = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
						let q0Label = GMisc.label ~markup:"<b>q<sub>0</sub></b>" ~packing:q0Box#add () in
						let q0Input = GEdit.entry ~text:q0 ~width:20 ~packing:q0Box#add () in
						GMisc.label ~text:"C" ~packing:q0Box#add ();
					let i0Box = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
           			let i0Label = GMisc.label ~markup:"<b>I<sub>0</sub></b>" ~packing:i0Box#add () in
            		let i0Input = GEdit.entry ~text:i0 ~width:20 ~packing:i0Box#add () in
            		GMisc.label ~markup:"A" ~packing:i0Box#add ();	
	let forcedRegimeTypeFrame = GBin.frame ~label:"Régime forcé" ~packing:dialogBox#vbox#add () in
		let forcedRegimeTypeFrameContent = GPack.vbox ~spacing:10 ~packing:forcedRegimeTypeFrame#add () in
			let regimeTypeBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
				GMisc.label ~markup:"<b>Type</b> : " ~packing:regimeTypeBox#add ();
				let regimesList = GEdit.combo_box_text ~strings:["Constant";"Sinusoïdal"] ~active:0 ~packing:regimeTypeBox#add () in
					let regimesListCBox = match regimesList with (x,y) -> x in
			let amplBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
				let amplLabel = GMisc.label ~markup:"<b>Amplitude</b> : " ~packing:amplBox#add () in
				let amplInput = GEdit.entry ~editable:false ~packing:amplBox#add () in
			let pulseBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
            let pulseLabel = GMisc.label ~markup:"<b>Pulsation</b> : " ~packing:pulseBox#add () in
				let pulseInput = GEdit.entry ~editable:false ~packing:pulseBox#add () in
			ignore (regimesListCBox#connect#changed ~callback:(fun () -> if regimesListCBox#active = 0 then (amplInput#set_editable false;amplInput#set_text "";pulseInput#set_editable false;pulseInput#set_text "") else (amplInput#set_editable true;pulseInput#set_editable true)));
   	let buttonBox = GPack.button_box `HORIZONTAL ~spacing:10 ~packing:dialogBox#vbox#add () in
	let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:buttonBox#add () in
    	backToMenuButton#connect#clicked ~callback:(fun () -> dialogBox#destroy ());
		GMisc.image ~stock:`GO_BACK ~packing:backToMenuButton#set_image ();
     	let validateButton = GButton.button ~label:"Valider" ~packing:buttonBox#add () in
	validateButton#connect#clicked ~callback:(
	fun () -> (
		(* Validation du formulaire *)
		let validationResult = process_elec_config rInput#text lInput#text cInput#text q0Input#text i0Input#text regimesListCBox#active pulseInput#text amplInput#text in
			(* Affichage des éventuelles erreurs *)
			if List.exists (fun x -> x = Invalid) (Array.to_list validationResult) then
   		begin
				messageLabel#set_text "<span foreground='red'>La configuration saisie présente des erreurs</span>";
				messageLabel#set_use_markup true;
			   (if Array.get validationResult 0 = Invalid then
			   begin
				   rLabel#set_text "<span foreground='red'><b>R</b> : </span>";
				   rLabel#set_use_markup true
			   end
			   else
				   rLabel#set_text "<b>R</b> : ";rLabel#set_use_markup true);
			   (if Array.get validationResult 1 = Invalid then
            begin
               lLabel#set_text "<span foreground='red'><b>L</b> : </span>";
               lLabel#set_use_markup true
            end    
            else
               lLabel#set_text "<b>L</b> : ";lLabel#set_use_markup true);
			   (if Array.get validationResult 2 = Invalid then
            begin
               cLabel#set_text "<span foreground='red'><b>C</b> : </span>";
               cLabel#set_use_markup true
            end    
            else
               cLabel#set_text "<b>C</b> : ";cLabel#set_use_markup true);
			   (if Array.get validationResult 3 = Invalid then
            begin
               q0Label#set_text "<span foreground='red'><b>q<sub>0</sub></b> : </span>";
               q0Label#set_use_markup true
            end    
            else
               q0Label#set_text "<b>q<sub>0</sub></b> : ";q0Label#set_use_markup true);
			   (if Array.get validationResult 4 = Invalid then
            begin
               i0Label#set_text "<span foreground='red'><b>i<sub>0</sub></b> : </span>";
               i0Label#set_use_markup true
            end    
            else
               i0Label#set_text "<b>i<sub>0</sub></b> : ";i0Label#set_use_markup true);
			   (if Array.get validationResult 5 = Invalid then
            begin
               pulseLabel#set_text "<span foreground='red'><b>Pulsation</b> : </span>";
               pulseLabel#set_use_markup true
            end    
            else
               pulseLabel#set_text "<b>Pulsation</b> : ";pulseLabel#set_use_markup true);
			   (if Array.get validationResult 6 = Invalid then
            begin
               amplLabel#set_text "<span foreground='red'><b>Amplitude</b> : </span>";
               amplLabel#set_use_markup true
            end    
            else
               amplLabel#set_text "<b>Amplitude</b> : ";amplLabel#set_use_markup true)
         end
		   else
		   begin
			   let r = rInput#text and l = lInput#text and c = cInput#text and q0 = q0Input#text and i0 = i0Input#text and forcedMode = regimesListCBox#active and pulse = pulseInput#text and ampl = amplInput#text in
            dialogBox#destroy ();
			   display_elec_results window r l c q0 i0 forcedMode pulse ampl
	      end
	));
	GMisc.image ~stock:`OK ~packing:validateButton#set_image ();
  dialogBox#show ()
;;

(******************************************************************************
*                          Menu principal 
******************************************************************************)

(**
* Affichage du menu principal
* @param window GWindow.window La fenêtre principale de l'application
*)
let menu window =
	(* GTK ne permet pas d'avoir plus d'un fils pour window,
	on créé donc un conteneur pour nos widgets *)
	let vbox = GPack.vbox
	~spacing:10
	~border_width:10
	~packing:window#add () in
   GMisc.label ~markup:"<span font_size='xx-large'><b>Bienvenue !</b></span>" ~packing:vbox#add ();
   GMisc.label ~markup:"<span font_size='large'>Veuillez choisir un type d'oscillateur :</span>" ~packing:vbox#add ();
   let frame = GBin.frame ~height:400 ~label:"Oscillateurs disponibles" ~packing:vbox#add () in
      let frameContent = GPack.hbox ~spacing:10 ~packing:frame#add () in
         let menuMeca = GPack.vbox ~spacing:10 ~packing:frameContent#add () in
		let buttonMBox = GPack.button_box `HORIZONTAL ~child_width:200 ~packing:menuMeca#add () in
			let buttonM = GButton.button
	         	~label:"Mécanique"
	         	~packing:buttonMBox#add () in
	         	buttonM#connect#clicked ~callback:(fun () -> start_meca_config window);
            let imgMeca = GMisc.image ~height:320 ~packing:menuMeca#add () in
            imgMeca#set_file "./img/oscillator_meca.png";
         let menuElec = GPack.vbox ~spacing:10 ~packing:frameContent#add () in
            let buttonEBox = GPack.button_box `HORIZONTAL ~child_width:200 ~packing:menuElec#add () in
		let buttonE = GButton.button
	         ~label:"Électrocinétique"
	         ~packing:buttonEBox#add () in
	         buttonE#connect#clicked ~callback:(fun () -> start_elec_config window);
            let imgElec = GMisc.image ~height:320 ~packing:menuElec#add () in
            imgElec#set_file "./img/oscillator_elec.png";
	 let quitButtonBox = GPack.button_box `HORIZONTAL ~packing:vbox#add () in
	 	let quitButton = GButton.button
         	~label:"Quitter"
         	~packing:quitButtonBox#add () in
            	quitButton#connect#clicked ~callback:(fun () -> quit window);
					GMisc.image ~stock:`QUIT ~packing:quitButton#set_image ()
;;

(**
* Programme principal de l'application (main)
*)
let _ =
  (* Création de la fenêtre principale de l'application *)
  let window = GWindow.window 
      ~title:"Oscillateur mécanique et électrocinétique" 
      ~height:560 
      ~width:900
      ~resizable:false () in
  (* Gestion de l'évènement « clic sur la croix de fermeture de la fenêtre *)
  window#connect#destroy ~callback:GMain.quit;
  menu window; (* Appel du menu principal *)
  window#show ();
  GMain.main ()
;;
