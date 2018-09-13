.PHONY: build doc install examples run clean dev-install dev-update dev-uninstall

all: build doc examples

build:
	dune build

doc:
	dune build @doc

install:
	dune install

examples:
	dune build @examples

example_progs := $(basename $(notdir $(wildcard ./examples/*.ml)))
run: examples
ifdef example
	dune exec ./examples/$(example).exe
else
	@$(info usage: make run example=counter)
	@$(info )
	@$(info available progs:)
	@$(foreach p,$(example_progs),$(info $(p)))
endif

clean:
	dune clean

dev-install:
	opam install . --working-dir

dev-update:
	opam upgrade . --working-dir

dev-uninstall:
	opam pin remove .
