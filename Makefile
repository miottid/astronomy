.PHONY: build test docs clean

build:
	dune build

test:
	dune build @test/runtest

docs:
	rm -fr docs
	mkdir -p docs
	dune build @doc
	cp -R _build/default/_doc/_html/* docs

clean:
	dune clean