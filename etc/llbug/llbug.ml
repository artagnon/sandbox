open Llvm
open Llvm_executionengine
open Llvm_target
open Ctypes

let () =
  assert (Llvm_executionengine.initialize ())

let context = global_context ()
let i8_type = Llvm.i8_type context
let i32_type = Llvm.i32_type context
let i64_type = Llvm.i64_type context
let double_type = Llvm.double_type context

let () =
  let m = create_module (global_context ()) "simple_module" in
  let ee = create m in
  let fn = define_function "scramble" (function_type i32_type [||]) m in
  let b = builder_at_end (global_context ()) (entry_block fn) in
  let add = const_int i32_type 24 in
  ignore (build_ret add b);
  let fn = define_function "ooo1" (function_type i32_type [||]) m in
  let b = builder_at_end (global_context ()) (entry_block fn) in
  let add = const_int i32_type 42 in
  ignore (build_ret add b);
  ignore (dump_module m);

  let cplusty = Foreign.funptr (void @-> returning int32_t) in
  let fh = get_function_address "ooo1" cplusty ee in
  print_int (Int32.to_int (fh ())); print_newline ();
