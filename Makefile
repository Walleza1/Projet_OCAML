.PHONY:build clean test create_png

build:
	ocamlbuild ftest.byte

clean:
	rm -rf _build *.byte

test:
	ocamlbuild test.byte && ./test.byte &&  make -s clean

create_png:
	ocamlbuild create_png.byte && ./create_png.byte &&  make -s clean
