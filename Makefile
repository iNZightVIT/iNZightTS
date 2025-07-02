R := R
RCMD := $(R) --slave

document:
	@$(RCMD) -e "devtools::document()"

test:
	@$(RCMD) -e "devtools::test()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "if (!requireNamespace('revdepcheck')) install.packages('revdepcheck')"
	@$(RCMD) -e "revdepcheck::revdep_check()"
	@$(RCMD) -f "revdep/check.R"

revcheck_reset:
	@$(RCMD) -e "revdepcheck::revdep_reset()"

crancheck: document
	@$(R) CMD build .
	@$(R) CMD check *.tar.gz

install:
	$(R) CMD INSTALL ./

clean:
	@rm -rf *.tar.gz *.Rcheck revdep

README.md: README.Rmd
	$(RCMD) -e "rmarkdown::render('$^')"
	rm README.html

BRANCH := $(shell git branch --show-current | sed 's/[a-z]*\///')
releasePRs:
	@echo Creating PR to master
	@-gh pr create -a "@me" -b "" -B master -l "release" -t "Release $(BRANCH)"
	@echo Creating PR to dev
	@gh pr create -a "@me" -b "" -B dev -l "release" -t "Release $(BRANCH) into dev"

site: README.md document install
	@cp NEWS.Md NEWS.md
	@$(RCMD) -e "pkgdown::build_site()"
