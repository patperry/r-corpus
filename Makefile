
CORPUS_LIB= src/corpus.so

all: $(CORPUS_LIB)

$(CORPUS_LIB):
	Rscript -e 'devtools::compile_dll(".")'

clean:
	Rscript -e 'devtools::clean_dll(".")'

check: $(CORPUS_LIB)
	Rscript -e 'Sys.setlocale(locale = "C"); devtools::test(".")'

install: $(CORPUS_LIB)
	Rscript -e 'devtools::install(".")'

.PHONY: all clean check
