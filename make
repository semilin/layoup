#!/bin/sh
sbcl --eval '(progn (asdf:load-asd (merge-pathnames "layoup.asd" (uiop:getcwd))) 
(asdf:make :layoup/cli) 
(quit))'
