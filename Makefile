# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0
# SPDX-License-Identifier: Unlicense

# Example Makefile that you can put into root directory of a Haskell package.
# We assume that it is called "mypackage".
# It uses `make/Makefile`.

.PHONY: all test-all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

all:
	$(MAKEU) PACKAGE=""
test:
	$(MAKEU) test PACKAGE=""
