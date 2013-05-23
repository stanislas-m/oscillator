#!/bin/sh
ocamlc -g -w s -I +lablgtk lablgtk.cma gtkInit.cmo main.ml -o test
