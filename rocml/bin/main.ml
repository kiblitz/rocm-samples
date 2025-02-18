open! Core

let[@inline] filepath_by_binary ~filename =
  let bin_path = Sys_unix.executable_name |> Filename.parts |> List.drop_last_exn in
  bin_path @ [ filename ] |> Filename.of_parts
;;

let lib =
  Dl.dlopen
    ~filename:(filepath_by_binary ~filename:"vectoradd_hip.so")
    ~flags:[ Dl.RTLD_LAZY ]
;;

let f = Foreign.foreign ~from:lib "f" Ctypes.(void @-> returning int)

let main () =
  [%my_ppx "yoo gabba gabba"];
  print_s [%message (f () : int)];
  print_endline "Hello, World!"
;;

main ()
