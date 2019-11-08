# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0
# SPDX-License-Identifier: Unlicense

# Example Makefile that you can put into root directory of a Haskell package.
# We assume that it is called "mypackage".
# It uses `make/Makefile`.

.PHONY: stkr-token test haddock haddock-no-deps stylish lint clean all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C ../make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=stkr-token

stkr-token:
	$(MAKE_PACKAGE) dev
test:
	$(MAKE_PACKAGE) test
test-ci:
	$(MAKE_PACKAGE) test-ci
test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term
test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
clean:
	$(MAKE_PACKAGE) clean

all: stkr-token
