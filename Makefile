RSCRIPT= Rscript --vanilla
CORPUS_LIB= src/corpus.so
BUILT_VIGNETTES= \
	vignettes/chinese.Rmd vignettes/corpus.Rmd vignettes/gender.Rmd \
	vignettes/stemmer.Rmd vignettes/textdata.Rmd

all: $(CORPUS_LIB) $(BUILT_VIGNETTES)

$(CORPUS_LIB):
	$(RSCRIPT) -e 'devtools::compile_dll(".")'

NEWS: NEWS.md
	sed -e 's/^### //g; s/`//g' $< > $@

README: README.md
	sed -e '/\*Corpus\*/,$$!d' \
		-e 's/…../.../' \
		-e 's/..…/.../' \
		-e 's/⋮/./' $< > $@

vignettes/%.Rmd: vignettes/%.Rmd.in
	$(RSCRIPT) -e 'devtools::load_all("."); setwd("vignettes"); knitr::knit(basename("$<"), basename("$@"))'

bench:
	$(RSCRIPT) -e 'devtools::load_all("."); source("bench/bench.R")'

check: $(CORPUS_LIB)
	$(RSCRIPT) -e 'devtools::test(".")'

clean:
	$(RSCRIPT) -e 'devtools::clean_dll(".")'

cov:
	$(RSCRIPT) -e 'covr::package_coverage(line_exclusions = c("R/deprecated.R", list.files("src/corpus", recursive = TRUE, full.names = TRUE)))'

dist: $(BUILT_VIGNETTES) NEWS README
	# work around CRAN's pragma check
	# https://github.com/wch/r-source/commit/b76c8fd355a0f5b23d42aaf44a879cac0fc31fa4#diff-4882a8c8e173bda109ed98da485e1428R8622
	mkdir -p dist
	cp src/corpus/src/table.c dist/table.c.orig
	sed 's/^\(#pragma GCC diagnostic .*\)/\/\/\1/g' dist/table.c.orig > \
		src/corpus/src/table.c
	cd dist && R CMD build ..
	mv dist/table.c.orig src/corpus/src/table.c

distclean: clean
	rm -rf $(BUILT_VIGNETTES)

doc: $(BUILT_VIGNETTES) NEWS README

install: $(CORPUS_LIB)
	$(RSCRIPT) -e 'devtools::install(".")'

site: $(BUILT_VIGNETTES)
	$(RSCRIPT) -e 'pkgdown::build_site(".")'

.PHONY: all bench check clean con dist distclean doc install site
