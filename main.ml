(**
* Mecanic or electromagnetic oscillator
*
* @author Alan BARD, Céline LEPICARD and Stanislas MICHALAK
* Require liblablgtk2-ocaml-dev
* To compile: ocaml -w s -I +lablgtk2 lablgtk.cma main.ml
*)

(* Initialisation de GTK. *)
let _ = GMain.init ()

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

let _ =
  window#connect#destroy ~callback:GMain.quit;
  window#show ();
  GMain.main ()
