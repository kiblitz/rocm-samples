open! Core

let width = 16
let height = 16
let num = width * height

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
    Ctypes.(ptr float @-> ptr float @-> ptr float @-> int @-> int @-> returning int)
;;

let init_carray ~f =
  let carray = Ctypes.CArray.make Ctypes.float num in
  for i = 0 to num - 1 do
    Ctypes.CArray.set carray i (f i)
  done;
  carray
;;

let[@kernel] g (x : _) (y : _) = x + y

let main () =
  let z = g 2 3 in
  print_s [%message "" (z : int)];
  [%my_ppx "yoo gabba gabba"];
  let arr_a, arr_b =
    let a = init_carray ~f:(fun i -> i * i |> Float.of_int) in
    let b = init_carray ~f:Float.of_int in
    a, b
  in
  let result = Ctypes.CArray.make Ctypes.float num in
  let raw_ptr carray = Ctypes.CArray.start carray in
  let start = Time_ns.now () in
  let err_cnt = f (raw_ptr arr_a) (raw_ptr arr_b) (raw_ptr result) width height in
  let end_ = Time_ns.now () in
  print_s [%message (err_cnt : int)];
  let elapsed = Time_ns.diff end_ start in
  print_s [%message (elapsed : Time_ns.Span.t)];
  print_endline "Hello, World!"
;;

main ()
