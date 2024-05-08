FROM ubuntu:18.04

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y --no-install-recommends sudo m4 make clang ocaml llvm-6.0-dev m4 cmake pkg-config aspcud ca-certificates python dos2unix vim opam

WORKDIR /plt2024

RUN opam init

RUN opam install llvm
