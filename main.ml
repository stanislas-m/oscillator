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
let test () = print_endline "Click";
flush stdout

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


let menuFrame = GBin.frame ~label:"Oscillateurs disponibles"
    ~packing:(vbox#pack ~expand:true ~fill:true ~padding:10) ~height:300 ()
let _ =
  window#connect#destroy ~callback:GMain.quit;
  window#show ();
  GMain.main ()
