build:
	dune build

clean:
	dune clean

doc:
	dune build @doc

test:
	dune test

watch:
	dune build --watch

.PHONY: build clean doc test watch
