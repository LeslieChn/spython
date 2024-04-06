.PHONY: all clean byte 

OCB_FLAGS = -I src -use-ocamlfind -lib unix
OCB = ocamlbuild $(OCB_FLAGS)

all: clean native

clean: 
	$(OCB) -clean
	rm -f a.out main *.ll *.s *out *.log *.diff spython

native:
	$(OCB) spython.native
	mv spython.native spython
