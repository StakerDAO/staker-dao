# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: Unlicense

# Defines utilities for other Makefiles

.PHONY: dev test build repl shell

# Build everything
dev:
	nix-shell --run 'cabal build'

end2end:
	nix-shell --run 'cabal run stkr-token-test-end2end'

# Run tests in all packages which have them.
test:
	nix-shell --run 'cabal test'

repl-exe:
	nix-shell --run 'cabal repl stkr-token-cli'

repl-test:
	nix-shell --run 'cabal repl stkr-token-test'

repl:
	nix-shell --run 'cabal repl'

shell:
	nix-shell

build:
	nix-build

run:
	nix-shell --run "cabal run stkr-token-exec"
