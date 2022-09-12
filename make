#!/bin/sh
sbcl --eval "(progn (asdf:make :layoup/cli) (quit))"
