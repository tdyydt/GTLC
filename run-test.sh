#!/bin/sh

# Compile
(
    cd src
    make depend
    make
)

# Run tests
(
    cd test
    make && ./test
)
