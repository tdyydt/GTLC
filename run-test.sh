#!/bin/sh

# Compile
(
    cd src
    omake
)

# Run tests
(
    cd test
    make && ./test
)
