#!/bin/sh

DEST=~/Work/webapp/gtlc-app/src
FILE=gtlc.js

cp $FILE $DEST
# Disable eslint for generated JS,
# because .eslintignore doesn't work with create-react-app ;(
ex -sc '1i|/* eslint-disable */' -cx "$DEST/$FILE"
