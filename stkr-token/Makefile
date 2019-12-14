# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0
# SPDX-License-Identifier: Unlicense

# Example Makefile that you can put into root directory of a Haskell package.
# We assume that it is called "mypackage".
# It uses `make/Makefile`.

.PHONY: dev test test-end2end all

CABAL_DEV_FLAGS =-O0 --disable-documentation

dev:
	cabal build $(CABAL_DEV_FLAGS) --enable-tests

test:
	cabal test $(CABAL_DEV_FLAGS)

test-end2end:
	cabal run stkr-token-end2end $(CABAL_DEV_FLAGS)

clean:
	cabal clean
