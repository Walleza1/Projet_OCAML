.PHONY:build clean test create_png

build:
	ocamlbuild ftest.byte

clean:
	rm -rf _build *.byte

test:
	ocamlbuild test.byte && ./test.byte &&  make -s clean
