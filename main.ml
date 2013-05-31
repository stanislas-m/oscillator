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
* Traitement des données 
*)

let processMecaConfig k l0 =
   print_endline k;
	print_endline l0;
   flush stdout
;;

let processElecConfig r l c e i =
   print_endline r;
	print_endline l;
	print_endline c;
	print_endline e;
	print_endline i;
   flush stdout
;;

(**
* Launchers 
*)

let startMecaConfig window =
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
	validateButton#connect#clicked ~callback:(fun () -> processMecaConfig kInput#text l0Input#text);
  dialogBox#show ()
;;

let startElecConfig window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur électrocinétique" ~height:480 ~width:750 ~parent:window ~modal:true ~destroy_with_parent:true () in
  let ownStartHbox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
  	let ownParamsFrame = GBin.frame ~label:"Paramètres propres" ~packing:ownStartHbox#add () in
  		let ownParamsFrameContent = GPack.vbox ~spacing:10 ~packing:ownParamsFrame#add () in
			let rBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				GMisc.label ~markup:"<b>R</b> : " ~packing:rBox#add ();
				let rInput = GEdit.entry ~width:20 ~packing:rBox#add () in
				GMisc.label ~text:"Ω" ~packing:rBox#add ();
			let lBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				GMisc.label ~markup:"<b>L</b> : " ~packing:lBox#add ();
				let lInput = GEdit.entry ~width:20 ~packing:lBox#add () in
				GMisc.label ~text:"H" ~packing:lBox#add ();
			let cBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				GMisc.label ~markup:"<b>C</b> : " ~packing:cBox#add ();
				let cInput = GEdit.entry ~width:20 ~packing:cBox#add () in
				GMisc.label ~text:"F" ~packing:cBox#add ();
			let eBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
				GMisc.label ~markup:"<b>E</b> : " ~packing:eBox#add ();
				let eInput = GEdit.entry ~width:20 ~packing:eBox#add () in
				GMisc.label ~text:"V" ~packing:eBox#add ();
			let iBox = GPack.hbox ~spacing:10 ~packing:ownParamsFrameContent#add () in
		   	GMisc.label ~markup:"<b>I</b> : " ~packing:iBox#add ();
		   	let iInput = GEdit.entry ~width:20 ~packing:iBox#add () in
		   	GMisc.label ~text:"A" ~packing:iBox#add ();
		let startConditionsFrame = GBin.frame ~label:"Conditions initiales" ~packing:ownStartHbox#add () in
	let forcedRegimeTypeFrame = GBin.frame ~label:"Régime forcé" ~packing:dialogBox#vbox#add () in
		let forcedRegimeTypeFrameContent = GPack.vbox ~spacing:10 ~packing:forcedRegimeTypeFrame#add () in
			let regimeTypeBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
				GMisc.label ~markup:"<b>Type</b> : " ~packing:regimeTypeBox#add ();
				let regimesList = GList.liste ~packing:regimeTypeBox#add () in
					let constRegime = GList.list_item ~label:"Constant" ~packing:regimesList#add () in
					constRegime#select ();
					GList.list_item ~label:"Sinusoïdal" ~packing:regimesList#add ();
			let amplBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
				GMisc.label ~markup:"<b>Amplitude</b> : " ~packing:amplBox#add ();
				let amplInput = GEdit.entry ~packing:amplBox#add () in
			let pulseBox = GPack.hbox ~spacing:10 ~packing:forcedRegimeTypeFrameContent#add () in
            GMisc.label ~markup:"<b>Pulsation</b> : " ~packing:pulseBox#add ();
	let amplInput = GEdit.entry ~packing:pulseBox#add () in
   let buttonBox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
  	let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:buttonBox#add () in
    	backToMenuButton#connect#clicked ~callback:(fun () -> dialogBox#destroy ());
     	let validateButton = GButton.button ~label:"Valider" ~packing:buttonBox#add () in
	validateButton#connect#clicked ~callback:(fun () -> processElecConfig rInput#text lInput#text cInput#text eInput#text iInput#text);
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

(* GTK ne permet pas d'avoir plus d'un fils pour window,
on créé donc un conteneur pour nos widgets *)
let vbox = GPack.vbox 
  ~spacing:10
  ~border_width:10
  ~packing:window#add ();;

(* Menu principal *)
let menu window vbox =
   GMisc.label ~markup:"<span font_size='xx-large'><b>Bienvenue !</b></span>" ~packing:vbox#add ();
   GMisc.label ~markup:"<span font_size='large'>Veuillez choisir un type d'oscillateur :</span>" ~packing:vbox#add ();
   let frame = GBin.frame ~height:400 ~label:"Oscillateurs disponibles" ~packing:vbox#add () in
      let frameContent = GPack.hbox ~spacing:10 ~packing:frame#add () in
         let menuMeca = GPack.vbox ~spacing:10 ~packing:frameContent#add () in
            let buttonM = GButton.button
	         ~label:"Mécanique"
	         ~packing:menuMeca#add () in
	         buttonM#connect#clicked ~callback:(fun () -> startMecaConfig window);
            let imgMeca = GMisc.image ~height:320 ~packing:menuMeca#add () in
            imgMeca#set_file "/usr/share/icons/gnome/48x48/categories/package_graphics.png";
         let menuElec = GPack.vbox ~spacing:10 ~packing:frameContent#add () in
            let buttonE = GButton.button
	         ~label:"Électrocinétique"
	         ~packing:menuElec#add () in
	         buttonE#connect#clicked ~callback:(fun () -> startElecConfig window);
            let imgElec = GMisc.image ~height:320 ~packing:menuElec#add () in
            imgElec#set_file "/usr/share/icons/gnome/48x48/categories/package_graphics.png";
         let quitButton = GButton.button
         ~label:"Quitter"
         ~packing:vbox#add () in
            quitButton#connect#clicked ~callback:GMain.quit         
;;

(* Main *)
let _ =
  window#connect#destroy ~callback:GMain.quit;
  menu window vbox;
  window#show ();
  GMain.main ()
;;
