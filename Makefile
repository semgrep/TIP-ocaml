all:
	dune build
clean:
	dune clean
setup:
	opam install -y --deps-only .
