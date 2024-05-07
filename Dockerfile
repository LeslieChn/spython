FROM ocaml/opam:ubuntu-20.04-ocaml-4.07
SHELL [ "/bin/bash", "-c" ]
ENV SHELL=/bin/bash
RUN DEBIAN_FRONTEND=noninteractive sudo apt update && sudo apt install -y -qq python2 make cmake clang llvm-10 vim dos2unix && opam pin llvm 10.0.0 && opam install ocamlbuild
WORKDIR /plt2024
