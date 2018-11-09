.PHONY:build clean

build:
	ocamlbuild ftest.byte

clean:
	rm -rf _build ftest.byte


