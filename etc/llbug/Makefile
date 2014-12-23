all: llbugc llbugcpp llbugml

llbugc: llbug.cpp
	clang++ -g -O1 $< `llvm-config --cxxflags --ldflags --system-libs --libs core bitwriter analysis native support mcjit` -o llbugc

llbugcpp: llbugplus.cpp
	clang++ -g -O1 $< `llvm-config --cxxflags --ldflags --system-libs --libs core bitwriter analysis native support mcjit` -o llbugcpp

llbugml: export OCAMLPATH = /Users/artagnon/src/llvm/Debug+Asserts/lib/ocaml
llbugml: llbug.ml
	ocamlfind ocamlopt -g -warn-error A -package llvm.executionengine -package llvm.bitwriter -linkpkg $< -o llbugml
