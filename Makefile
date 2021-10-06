# Always run `make shell` first to enter a Nix shell. If you start
# Emacs from within a Nix shell then its Haskell support works with
# the project.
#

PROJECT="paperspan2instapaper"

all: build

# Always use `make shell` first, before other commands.
shell:
	LC_ALL=C.UTF-8 nix-shell

build:
	hpack
	cabal new-build

rebuild:
	cabal new-clean
	rm $(PROJECT).cabal
	hpack
	cabal new-build

clean:
	cabal new-clean

repl:
	cabal new-repl

ls:
	@cabal list-bin ${PROJECT}

lint:
	hlint -v `ag --haskell -l`