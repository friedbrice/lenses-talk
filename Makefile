RESOLVER     = --resolver lts-13.7
RANDOM       = --package random
LENS         = --package lens
GENERIC_LENS = --package generic-lens
INTRODUCTION = LensesIntroduction.hs
OVERVIEW     = LensLibraryOverview.hs
USING        = UsingTheLensLibrary.hs

.PHONY: list
list: ## Show available targets.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONEY: repl-intro
repl-intro: ## Load LensesIntroduction.hs in ghci.
	@which stack > /dev/null
	@stack repl $(RESOLVER) $(RANDOM) $(INTRODUCTION)

.PHONY: watch-intro
watch-intro: ## Watch LensesIntroduction.hs in ghcid.
	@which ghcid > /dev/null
	@ghcid --command="stack repl $(RESOLVER) $(RANDOM)" $(INTRODUCTION)

.PHONY: repl-overview
repl-overview: ## Load LensLibraryOverview.hs in ghci.
	@which stack > /dev/null
	@stack repl $(RESOLVER) $(LENS) $(OVERVIEW)

.PHONY: watch-overview
watch-overview: ## Watch LensLibraryOverview.hs is ghcid.
	@which ghcid > /dev/null
	@ghcid --command="stack repl $(RESOLVER) $(LENS)" $(OVERVIEW)

.PHONY: repl-using
repl-using: ## Load UsingTheLensLibrary.hs in ghci.
	@which stack > /dev/null
	@stack repl $(RESOLVER) $(LENS) $(GENERIC_LENS) $(USING)

.PHONY: watch-using
watch-using: ## Watch UsingTheLensLibrary.hs in ghcid.
	@which ghcid > /dev/null
	@ghcid --command="stack repl $(RESOLVER) $(LENS) $(GENERIC_LENS)" $(USING)

.PHONY: test
test: ## Test-compile Haskell source files.
	@which stack > /dev/null
	@stack ghc $(RESOLVER) $(RANDOM) $(INTRODUCTION)
	@stack ghc $(RESOLVER) $(LENS) $(OVERVIEW)
	@stack ghc $(RESOLVER) $(LENS) $(GENERIC_LENS) $(USING)
	@rm *.hi
	@rm *.o
