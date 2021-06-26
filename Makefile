all: build

dev:
	ghcid --command "ghci app/Main -fobject-code -i.:src:app" --test main
build:
	nix-build default.nix
