RSCRIPT= Rscript --vanilla
CORPUS_LIB= src/corpus.so
BUILT_VIGNETTES= vignettes/chinese.Rmd

all: $(CORPUS_LIB) $(BUILT_VIGNETTES)

$(CORPUS_LIB):
	$(RSCRIPT) -e 'devtools::compile_dll(".")'

vignettes/chinese.Rmd: vignettes/src/chinese.Rmd
	rm -rf vignettes/fig/chinese-*
	cd vignettes && $(RSCRIPT) -e 'knitr::knit("src/chinese.Rmd")'
	mv vignettes/chinese.md $@
	touch -r $< $@ # copy the source file's time stamp

bench:
	$(RSCRIPT) -e 'devtools::load_all("."); source("bench/bench.R")'

clean:
	$(RSCRIPT) -e 'devtools::clean_dll(".")'
	rm -rf $(BUILT_VIGNETTES)

check: $(CORPUS_LIB)
	$(RSCRIPT) -e 'Sys.setlocale(locale = "C"); devtools::test(".")'

dist: vignettes
	mkdir -p dist && cd dist && R CMD build ..

doc:
	$(RSCRIPT) -e 'rmarkdown::render("README.Rmd", output_format=c("html_document", "md_document"))'

vignettes: $(BUILT_VIGNETTES)

install: $(CORPUS_LIB)
	$(RSCRIPT) -e 'devtools::install(".")'

.PHONY: all bench clean check dist doc install vignettes
