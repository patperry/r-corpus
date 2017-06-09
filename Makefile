
CORPUS_LIB= src/corpus.so

all: $(CORPUS_LIB)

$(CORPUS_LIB):
	Rscript -e 'devtools::compile_dll(".")'

bench:
	Rscript -e 'devtools::load_all("."); source("bench/bench.R")'

clean:
	Rscript -e 'devtools::clean_dll(".")'

check: $(CORPUS_LIB)
	Rscript -e 'Sys.setlocale(locale = "C"); devtools::test(".")'

dist:
	mkdir -p dist && cd dist && R CMD build ..

install: $(CORPUS_LIB)
	Rscript -e 'devtools::install(".")'

.PHONY: all bench clean check dist install
