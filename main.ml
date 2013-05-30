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

(**
* Accesseurs 
*)

(**
* Gestion de l'affichage
*)

(* Fenêtre principale de l'application. *)
let window = GWindow.window 
  ~title:"Oscillateur mécanique et électrocinétique" 
  ~height:460 
  ~width:800 ()

(* GTK ne permet pas d'avoir plus d'un fils pour window,
on créé donc un conteneur pour nos widgets *)
let vbox = GPack.vbox 
  ~spacing:10
  ~border_width:10
  ~packing:window#add ()

let mainTitle = GMisc.label ~markup:"<span font_size='xx-large'><b>Bienvenue !</b></span>" ~packing:vbox#add ()

let subTitle = GMisc.label ~markup:"<span font_size='large'>Veuillez choisir un type d'oscillateur :</span>" ~packing:vbox#add ()

let frame = GBin.frame ~height:300 ~label:"Oscillateurs disponibles" ~packing:vbox#add ()
let frameContent = GPack.hbox ~spacing:10 ~packing:frame#add ()
let menuMeca = GPack.vbox ~spacing:10 ~packing:frameContent#add ()
let menuElec = GPack.vbox ~spacing:10 ~packing:frameContent#add () 
let buttonM = GButton.button
	~label:"Mécanique"
	~packing:menuMeca#add ()
let imgMeca = GMisc.label ~text:"Img meca" ~height:240 ~packing:menuMeca#add ()
let buttonE = GButton.button
	~label:"Électrocinétique"
	~packing:menuElec#add ()
let imgElec = GMisc.label ~text:"Img elec" ~height:240 ~packing:menuElec#add ()
let quitButton = GButton.button ~label:"Quitter" ~packing:vbox#add ()
let _ =
  window#connect#destroy ~callback:GMain.quit;
  window#show ();
  GMain.main ()
