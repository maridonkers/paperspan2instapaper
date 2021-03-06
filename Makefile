# Always run `make shell` first to enter a Nix shell. If you start
# Emacs from within a Nix shell then its Haskell support works with
# the project.
#

PROJECT="paperspan2instapaper"
EXECUTABLE="Paperspan2Instapaper"

all: help

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

update:
	cabal new-update

clean:
	cabal new-clean

repl:
	cabal new-repl

ls:
	@cabal list-bin ${EXECUTABLE}

lint:
	ag --haskell -l | xargs hlint -v

formatter:
	ag --haskell -l | xargs stylish-haskell -i

help:
	@grep '^[^ 	#:]\+:' Makefile | sed -e 's/:[^:]*//g'
