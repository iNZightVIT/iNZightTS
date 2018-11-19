R := R
RCMD := $(R) --slave

document:
	@echo Roxygenizing documentation ...
	@echo
	@$(RCMD) -e "devtools::document()"
	@echo

check: document
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "devtools::use_revdep()"
	@$(RCMD) -f "revdep/check.R"

crancheck: document
	@echo Running CRAN checks using r-devel ...
	@echo
	@$(Rdev) CMD build .
	@$(Rdev) CMD check *.tar.gz
	@echo

install: document
	@echo Installing locally ...
	@echo
	$(R) CMD INSTALL ./
	@echo

clean:
	@rm -rf *.tar.gz *.Rcheck revdep
