all: llbugc llbugcpp llbugml

llbugc: llbug.cpp
	clang++ -g -O1 $< `llvm-config --cxxflags --ldflags --system-libs --libs core analysis native support mcjit` -o llbugc

llbugcpp: llbugplus.cpp
	clang++ -g -O1 $< `llvm-config --cxxflags --ldflags --system-libs --libs core native mcjit` -o llbugcpp

llbugml: export OCAMLPATH = /Users/artagnon/src/llvm/Debug+Asserts/lib/ocaml
llbugml: llbug.ml
	ocamlfind ocamlopt -g -warn-error A -package llvm.executionengine -linkpkg $< -o llbugml

clean:
	rm llbugc llbugcpp llbugml
