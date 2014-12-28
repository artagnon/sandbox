LLVM_CONFIG=~/src/llvm/build/bin/llvm-config

all: llbugc llbugcpp llbugml

llbugc: llbug.cpp
	clang++ -g -O1 $< `$(LLVM_CONFIG) --cxxflags --ldflags --system-libs --libs core analysis native support mcjit` -o llbugc

llbugcpp: llbugplus.cpp
	clang++ -g -O1 $< `$(LLVM_CONFIG) --cxxflags --ldflags --system-libs --libs core native mcjit` -o llbugcpp

llbugml: export OCAMLPATH = ${HOME}/src/llvm/build/lib/ocaml
llbugml: llbug.ml
	ocamlfind ocamlopt -g -warn-error A -package llvm.executionengine -linkpkg $< -o llbugml

clean:
	rm -f llbugc llbugcpp llbugml
