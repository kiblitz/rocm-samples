open! Core
open Ctypes
open Foreign

let lib = Dl.dlopen ~filename:"vectoradd_hip.so" ~flags:[ Dl.RTLD_NOW ]
let f = foreign ~from:lib "f" (void @-> returning int)

let main () =
  [%my_ppx "yoo gabba gabba"];
  print_s [%message (f () : int)];
  print_endline "Hello, World!"
;;

main ()
