open! Core

let num = 1024 * 1024

let[@inline] filepath_by_binary ~filename =
  let bin_path = Sys_unix.executable_name |> Filename.parts |> List.drop_last_exn in
  bin_path @ [ filename ] |> Filename.of_parts
;;

let lib =
  Dl.dlopen
    ~filename:(filepath_by_binary ~filename:"vectoradd_hip.so")
    ~flags:[ Dl.RTLD_LAZY ]
;;

let f =
  Foreign.foreign
    ~from:lib
    "f"
    Ctypes.(ptr float @-> ptr float @-> ptr float @-> returning int)
;;

let init_carray ~f =
  let carray = Ctypes.CArray.make Ctypes.float num in
  for i = 0 to num - 1 do
    Ctypes.CArray.set carray i (f i)
  done;
  carray
;;

let main () =
  [%my_ppx "yoo gabba gabba"];
  let arr_a, arr_b =
    let a = init_carray ~f:(fun i -> i * i |> Float.of_int) in
    let b = init_carray ~f:Float.of_int in
    a, b
  in
  let result = Ctypes.CArray.make Ctypes.float num in
  let raw_ptr carray = Ctypes.CArray.start carray in
  print_s [%message (f (raw_ptr arr_a) (raw_ptr arr_b) (raw_ptr result) : int)];
  for i = 0 to num - 1 do
    let v = Ctypes.CArray.get result i in
    print_s [%message (i : int) (v : float)]
  done;
  print_endline "Hello, World!"
;;

main ()
