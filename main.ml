(**
* Mecanic or electromagnetic oscillator
*
* @author Alan BARD, Céline LEPICARD and Stanislas MICHALAK
* To compile: ocamlc graphics.cma main.ml -o oscillateur
*)

open GMain
open GdkKeysyms

(*************************
* Main : boucle principale
**************************)
let main () =
  let window = GWindow.window ~width:320 ~height:240
                 ~title:"Oscillateur mécanique et électromagnétique" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accelGroup = factory#accel_group in
  let fileMenu = factory#add_submenu "Fichier" in

  (* File menu *)
  let factory = new GMenu.factory fileMenu ~accel_group in
  factory#add_item "Quitter" ~key:_Q ~callback: Main.quit;

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accelGroup;
  window#show ();
  Main.main ()

let () = main ()
