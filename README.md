Corpus (R package)
==================

Text corpus analysis in R. Heavy lifting is done by the
[Corpus C library][corpus].

[corpus]: https://github.com/patperry/corpus


Installing
----------

This package uses a git submodule, so you cannot use
`devtools::install_github` to install it. Instead, use the following command:

    devtools::install_git("git://github.com/patperry/r-corpus.git", args="--recursive")


Building from source
--------------------

To build the library from source, clone the repo and the submodules:

    git clone --recursive git://github.com/patperry/r-corpus.git

The `--recursive` flag is to make sure that the corpus library also gets
cloned. If you forget the `--recursive` flag, you can manually cloan
the submodule with the following command:

    git submodule update --init

There are no dependencies, but to build and run the tests, you will need
to install the [Check Unit Testing][check] library.

[check]:https://libcheck.github.io/check/
