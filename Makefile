# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: Unlicense

# Defines utilities for other Makefiles

.PHONY: dev test build repl shell end2end

# Build everything
dev:
	nix-shell --run 'cd stkr-token && hpack && cabal build -O0'

# Run tests in all packages which have them.
test:
	nix-shell --run 'cd stkr-token && cabal test'

repl-exe:
	nix-shell --run 'cd stkr-token && cabal repl stkr-token-cli'

repl-test:
	nix-shell --run 'cd stkr-token && cabal repl stkr-token-test'

repl:
	nix-shell --run 'cd stkr-token && cabal repl'

shell:
	nix-shell

build:
	nix-build -A stkr-token-cli

run:
	nix-shell --run "cd stkr-token && cabal run stkr-token-cli"

end2end:
	@if [[ -z "$$TEZOS_CLIENT" ]]; then nix-build end2end.nix --arg useTezosDerivation true && ./result; \
	else nix-build end2end.nix --arg useTezosDerivation false --argstr tezosClientPath "$$TEZOS_CLIENT" && ./result; \
	fi
