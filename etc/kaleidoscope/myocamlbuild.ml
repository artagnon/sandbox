open Ocamlbuild_plugin;;

  ocaml_lib ~extern:true ~dir:"/Users/artagnon/.opam/system/lib/llvm" "llvm";;
    ocaml_lib ~extern:true
	      ~dir:"/Users/artagnon/.opam/system/lib/llvm" "llvm_analysis";;

      flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++"]);;
