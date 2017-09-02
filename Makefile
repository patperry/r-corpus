RSCRIPT= Rscript --vanilla
CORPUS_LIB= src/corpus.so
BUILT_VIGNETTES= vignettes/chinese.Rmd vignettes/corpus.Rmd vignettes/gender.Rmd vignettes/unicode.Rmd

all: $(CORPUS_LIB) $(BUILT_VIGNETTES)

$(CORPUS_LIB):
	$(RSCRIPT) -e 'devtools::compile_dll(".")'

NEWS: NEWS.md
	sed -e 's/^### //g; s/`//g' $< > $@

README: README.md
	sed -e '/Overview/,$$!d' $< > $@

vignettes/chinese.Rmd: vignettes/chinese.Rmd.in
	$(RSCRIPT) -e 'devtools::load_all("."); setwd("vignettes"); knitr::knit("chinese.Rmd.in")'
	mv vignettes/chinese.Rmd.txt $@

vignettes/corpus.Rmd: vignettes/corpus.Rmd.in
	$(RSCRIPT) -e 'devtools::load_all("."); setwd("vignettes"); knitr::knit("corpus.Rmd.in")'
	mv vignettes/corpus.Rmd.txt $@

vignettes/gender.Rmd: vignettes/gender.Rmd.in
	$(RSCRIPT) -e 'devtools::load_all("."); setwd("vignettes"); knitr::knit("gender.Rmd.in")'
	mv vignettes/gender.Rmd.txt $@

vignettes/unicode.Rmd: vignettes/unicode.Rmd.in
	$(RSCRIPT) -e 'devtools::load_all("."); setwd("vignettes"); knitr::knit("unicode.Rmd.in")'
	mv vignettes/unicode.Rmd.txt $@

bench:
	$(RSCRIPT) -e 'devtools::load_all("."); source("bench/bench.R")'

check: $(CORPUS_LIB)
	$(RSCRIPT) -e 'devtools::test(".")'

clean:
	$(RSCRIPT) -e 'devtools::clean_dll(".")'
	rm -rf $(BUILT_VIGNETTES)

cov:
	$(RSCRIPT) -e 'covr::package_coverage(line_exclusions = c("R/deprecated.R", list.files("src/corpus", recursive = TRUE, full.names = TRUE)))'

dist: $(BUILT_VIGNETTES) NEWS README
	mkdir -p dist && cd dist && R CMD build ..

doc: $(BUILT_VIGNETTES) NEWS README

install: $(CORPUS_LIB)
	$(RSCRIPT) -e 'devtools::install(".")'

site:
	$(RSCRIPT) -e 'pkgdown::build_site(".")'

.PHONY: all bench check clean con dist doc install site
