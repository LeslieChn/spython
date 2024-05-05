.PHONY: all clean byte native fn

OCB_FLAGS = -I src -lib unix -use-ocamlfind

OCB = dos2unix ./testall.sh && ocamlbuild $(OCB_FLAGS) 

all: clean native 

clean:
	$(OCB) -clean
	rm -f a.out main *.ll *.s *.out *.log *.diff spython

native:
	$(OCB) spython.native
	mv spython.native spython 

test: native
	./testall.sh
