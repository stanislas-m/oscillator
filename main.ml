(**
* Mecanic or electromagnetic oscillator
*
* @author Alan BARD, Céline LEPICARD and Stanislas MICHALAK
* Require liblablgtk2-ocaml-dev
* To compile: ocaml -w s -I +lablgtk2 lablgtk.cma main.ml
*)

(* Initialisation de GTK. *)
let _ = GMain.init ()

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

(**
* Affichage des résultats
*)

let display_meca_results window =
 ()
;;

let display_elec_results window =
 ()
;;

(**
* Validation des données 
*)

type validField = Valid | Invalid;;

let process_meca_config k l0 =
   print_endline k;
	print_endline l0;
   flush stdout
;;

let process_elec_config r l c e i forcedMode pulse amp =
   [|(try
		if int_of_string r < 0 then failwith "int_of_string" else Valid
	with Failure "int_of_string" -> Invalid); 
	(try
		if int_of_string l < 0 then failwith "int_of_string" else Valid
	with Failure "int_of_string" -> Invalid);
	(try
		if int_of_string c < 0 then failwith "int_of_string" else Valid
	with Failure "int_of_string" -> Invalid);
	(try
		if int_of_string e < 0 then failwith "int_of_string" else Valid
	with Failure "int_of_string" -> Invalid);
	(try
		if int_of_string i < 0 then failwith "int_of_string" else Valid
	with Failure "int_of_string" -> Invalid);
	(if forcedMode = 1 then
		(try
         if int_of_string pulse < 0 then failwith "int_of_string" else Valid
      with Failure "int_of_string" -> Invalid)
	else
		Valid);
	(if forcedMode = 1 then
      (try
         if int_of_string amp < 0 then failwith "int_of_string" else Valid
      with Failure "int_of_string" -> Invalid)
   else
      Valid)
	|]
;;

(**
* Launchers 
*)

let start_meca_config window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur mécanique" ~height:360 ~width:600 ~parent:window ~modal:true ~destroy_with_parent:true () in
  let ownStartHBox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
   let ownParamsFrame = GBin.frame ~label:"Paramètres propres" ~packing:ownStartHBox#add () in
	let frameContent = GPack.vbox ~spacing:10 ~packing:ownParamsFrame#add () in
	let kBox = GPack.hbox ~spacing:10 ~packing:frameContent#add () in
		GMisc.label ~text:"k : " ~packing:kBox#add ();
		let kInput = GEdit.entry ~packing:kBox#add () in
		GMisc.label ~text: " " ~packing:kBox#add ();	
	let l0Box = GPack.hbox~spacing:10 ~packing:frameContent#add () in
		GMisc.label ~text:"l0 : " ~packing:l0Box#add ();
		let l0Input = GEdit.entry ~packing:l0Box#add () in
		GMisc.label ~text: "cm" ~packing:l0Box#add ();
	let lambdaBox = GPack.hbox~spacing:10 ~packing:frameContent#add () in
		GMisc.label ~text:"λ : " ~packing:lambdaBox#add ();
		let lambdaInput = GEdit.entry ~packing:lambdaBox#add () in
		GMisc.label ~text: " " ~packing:lambdaBox#add ();
   	let initCondFrame = GBin.frame ~label:"Conditions initiales" ~packing:ownStartHBox#add () in
   let regimeForce = GBin.frame ~label:"Type de régime forcé" ~packing:dialogBox#vbox#add () in
	let regimeForceContent = GPack.hbox ~spacing:10 ~packing:regimeForce#add () in
	  let regimeList = GList.liste ~packing:regimeForceContent#add () in
		let constRegi = GList.list_item ~label:"Constant" ~packing:regimeList#add () in
		constRegi#select ();
		GList.list_item ~label:"Sinusoïdal" ~packing:regimeList#add ();
   let buttonBox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
   	let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:buttonBox#add () in
	backToMenuButton#connect#clicked ~callback:(fun () -> dialogBox#destroy ());
	let validateButton = GButton.button ~label:"Valider" ~packing:buttonBox#add () in
	validateButton#connect#clicked ~callback:(fun () -> process_meca_config kInput#text l0Input#text);
  dialogBox#show ()
;;

let start_elec_config window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur électrocinétique" ~height:480 ~width:750 ~parent:window ~modal:true ~destroy_with_parent:true () in
  	let messageLabel = GMisc.label ~packing:dialogBox#vbox#add () in
	let ownStartHbox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
  	let ownParamsFrame = GBin.frame ~label:"Paramètres propres" ~packing:ownStartHbox#add () in
  		let ownParamsFrameContent = GPack.vbox ~spacing:10 ~packing:ownParamsFrame#add () in
			let rBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let rLabel = GMisc.label ~markup:"<b>R</b> : " ~packing:rBox#add () in
				let rInput = GEdit.entry ~width:20 ~packing:rBox#add () in
				GMisc.label ~text:"Ω" ~packing:rBox#add ();
			let lBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let lLabel = GMisc.label ~markup:"<b>L</b> : " ~packing:lBox#add () in
				let lInput = GEdit.entry ~width:20 ~packing:lBox#add () in
				GMisc.label ~text:"H" ~packing:lBox#add ();
			let cBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let cLabel = GMisc.label ~markup:"<b>C</b> : " ~packing:cBox#add () in
				let cInput = GEdit.entry ~width:20 ~packing:cBox#add () in
				GMisc.label ~text:"F" ~packing:cBox#add ();
			let eBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				let eLabel = GMisc.label ~markup:"<b>E</b> : " ~packing:eBox#add () in
				let eInput = GEdit.entry ~width:20 ~packing:eBox#add () in
				GMisc.label ~text:"V" ~packing:eBox#add ();
			let iBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
		   	let iLabel = GMisc.label ~markup:"<b>I</b> : " ~packing:iBox#add () in
		   	let iInput = GEdit.entry ~width:20 ~packing:iBox#add () in
		   	GMisc.label ~text:"A" ~packing:iBox#add ();
		let initCondFrame = GBin.frame ~label:"Conditions initiales" ~packing:ownStartHbox#add () in
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
		let validationResult = process_elec_config rInput#text lInput#text cInput#text eInput#text iInput#text regimesListCBox#active pulseInput#text amplInput#text in
			(* Affichage des éventuelles erreurs *)
			(if List.exists (fun x -> x = Invalid) (Array.to_list validationResult) then
			begin
				messageLabel#set_text "<span foreground='red'>La configuration saisie présente des erreurs</span>";
				messageLabel#set_use_markup true
			end
			else
				messageLabel#set_text "");
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
            eLabel#set_text "<span foreground='red'><b>E</b> : </span>";
            eLabel#set_use_markup true
         end    
         else
            eLabel#set_text "<b>E</b> : ";eLabel#set_use_markup true);
			(if Array.get validationResult 4 = Invalid then
         begin
            iLabel#set_text "<span foreground='red'><b>I</b> : </span>";
            iLabel#set_use_markup true
         end    
         else
            iLabel#set_text "<b>I</b> : ";iLabel#set_use_markup true);
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
            amplLabel#set_text "<b>Amplitude</b> : ";amplLabel#set_use_markup true);
	)
	);
	GMisc.image ~stock:`OK ~packing:validateButton#set_image ();
  dialogBox#show ()
;;

(**
* Gestion de l'affichage
*)

(* Fenêtre principale de l'application. *)
let window = GWindow.window 
  ~title:"Oscillateur mécanique et électrocinétique" 
  ~height:560 
  ~width:900 ();;

(* Menu principal *)
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

(* Main *)
let _ =
  window#connect#destroy ~callback:GMain.quit;
  menu window;
  window#show ();
  GMain.main ()
;;
