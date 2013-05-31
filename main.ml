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

let processMecaConfig () =
   ()
;;

let processElecConfig () =
   ()
;;

(**
* Launchers 
*)

let startMecaConfig window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur mécanique" ~height:360 ~width:600 ~parent:window ~modal:true ~destroy_with_parent:true () in
   let buttonBox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
   	let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:buttonBox#add () in
   	backToMenuButton#connect#clicked ~callback:(fun () -> dialogBox#destroy ());
	let validateButton = GButton.button ~label:"Valider" ~packing:buttonBox#add () in
	validateButton#connect#clicked ~callback:(fun () -> processMecaConfig ());
  dialogBox#show ()
;;

let startElecConfig window =
  let dialogBox = GWindow.dialog ~title:"Configuration d'un oscillateur électrocinétique" ~height:360 ~width:600 ~parent:window ~modal:true ~destroy_with_parent:true () in
  let buttonBox = GPack.hbox ~spacing:10 ~packing:dialogBox#vbox#add () in
  	let backToMenuButton = GButton.button ~label:"Retour au menu" ~packing:buttonBox#add () in
    	backToMenuButton#connect#clicked ~callback:(fun () -> dialogBox#destroy ());
     	let validateButton = GButton.button ~label:"Valider" ~packing:buttonBox#add () in
	validateButton#connect#clicked ~callback:(fun () -> processElecConfig ());
  dialogBox#show ()
;;

(**
* Gestion de l'affichage
*)

(* Fenêtre principale de l'application. *)
let window = GWindow.window 
  ~title:"Oscillateur mécanique et électrocinétique" 
  ~height:460 
  ~width:800 ();;

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
   let frame = GBin.frame ~height:300 ~label:"Oscillateurs disponibles" ~packing:vbox#add () in
      let frameContent = GPack.hbox ~spacing:10 ~packing:frame#add () in
         let menuMeca = GPack.vbox ~spacing:10 ~packing:frameContent#add () in
            let buttonM = GButton.button
	         ~label:"Mécanique"
	         ~packing:menuMeca#add () in
	         buttonM#connect#clicked ~callback:(fun () -> startMecaConfig window);
            let imgMeca = GMisc.image ~height:240 ~packing:menuMeca#add () in
            imgMeca#set_file "/usr/share/icons/gnome/48x48/categories/package_graphics.png";
         let menuElec = GPack.vbox ~spacing:10 ~packing:frameContent#add () in
            let buttonE = GButton.button
	         ~label:"Électrocinétique"
	         ~packing:menuElec#add () in
	         buttonE#connect#clicked ~callback:(fun () -> startElecConfig window);
            let imgElec = GMisc.image ~height:240 ~packing:menuElec#add () in
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
