update:
	cabal2nix . > project.nix

shell: update
	nix-shell

run: update
	nix-shell --run "cabal configure; cabal run"

build: update
	nix-build default.nix

.PHONY: gen clash
gen:
	nix-shell ~/dev/hw/clash/clash-compiler-1.2.3/shell.nix --run "sh gen.sh"

clash:
	nix-shell ~/dev/hw/clash/clash-compiler-1.2.3/shell.nix

clean:
	rm -rf cabal.*
	rm -rf dist*
	rm -rf result
