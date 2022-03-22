PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

ifeq ($(UNAME), Darwin)
	STACK := stack --no-nix
else 
	STACK := stack
endif

all: setup build

install:
	$(STACK) install
	juvix fetch-stdlibs

fetch-stdlibs: 
	cd library/StandardLibrary && $(STACK) build && $(STACK) exec -- fetch-libs

update-local-stdlibs:
	mkdir -p $(HOME)/.juvix/stdlib
	cp -rp stdlib/* $(HOME)/.juvix/stdlib

setup:
	$(STACK) build --only-dependencies --jobs $(THREADS)

build-libff:
	./scripts/build-libff.sh

build-z3:
	mkdir -p $(PREFIX)
	cd z3 && test -f build/Makefile || python scripts/mk_make.py -p $(PREFIX)
	cd z3/build && make -j $(PREFIX)
	cd z3/build && make install

build: 
	$(STACK) build --fast --jobs $(THREADS)

build-watch:
	$(STACK) build --fast --file-watch

build-prod: clean
	$(STACK) build --jobs $(THREADS) --ghc-options="-O3" --ghc-options="-fllvm" --flag juvix:incomplete-error

build-format:
	$(STACK) install ormolu

lint:
	$(STACK) exec -- hlint app src test

format:
	find . -path ./.stack-work -prune -o -path ./archived -prune -o -type f -name "*.hs" -exec ormolu --mode inplace {} --ghc-opt -XTypeApplications --ghc-opt -XUnicodeSyntax --ghc-opt -XPatternSynonyms --ghc-opt -XTemplateHaskell \;

org-gen:
	org-generation app/ docs/Code/App.org test/ docs/Code/Test.org src/ docs/Code/Juvix.org bench/ docs/Code/Bench.org library/ docs/Code/Library.org

test:
	$(STACK) test --fast --jobs=$(THREADS) --test-arguments "--hide-successes --ansi-tricks false"

test-parser: build update-local-stdlibs
	find test/examples/positive/llvm -name "*.ju" | xargs -t -n 1 -I % $(STACK) exec juvix -- parse % 


test-typecheck: build update-local-stdlibs
	find test/examples/positive/llvm -name "*.ju" | xargs -t -n 1 -I % $(STACK) exec juvix -- typecheck % -b "llvm"

test-compile: build update-local-stdlibs
	find test/examples/positive/llvm -maxdepth 1 -name "*.ju" | xargs -n 1 -I % basename % .ju | xargs -t -n 1 -I % $(STACK) exec juvix -- compile test/examples/positive/llvm/%.ju test/examples/positive/llvm/%.ll -b "llvm"
	rm test/examples/positive/llvm/*.ll

bench:
	$(STACK) bench --benchmark-arguments="--output ./docs/Code/bench.html"

repl-lib:
	$(STACK) ghci juvix:lib

repl-exe:
	$(STACK) ghci juvix:exe:juvix

clean:
	$(STACK) clean

clean-full:
	$(STACK) clean --full

stack-yaml:
	ros -Q scripts/yaml-generator/yaml-generator.asd

# Overwrite existing golden files
accept-golden:
	rm -rf test/examples-golden/positive
	rm -rf test/examples-golden/negative
	# If we want further commands, put - at the start, to ignore
	# the error from this command
	$(STACK) test --test-arguments "--accept"


.PHONY: all setup build build-libff build-z3 build-watch build-prod lint format org-gen test test-parser test-typecheck test-compile repl-lib repl-exe clean clean-full bench build-format
