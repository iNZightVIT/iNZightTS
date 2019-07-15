R := R
RCMD := Rscript

document:
	@echo Roxygenizing documentation ...
	@echo
	@$(RCMD) -e "devtools::document()"
	@echo

check: document
	@$(RCMD) -e "devtools::check()"

test:
	@$(RCMD) -e "devtools::test()"

revcheck:
	@$(RCMD) -e "devtools::use_revdep()"
	@$(RCMD) "revdep/check.R"

install: document
	@echo Installing locally ...
	@echo
	$(R) CMD INSTALL ./
	@echo

clean:
	@rm -rf *.tar.gz *.Rcheck revdep
