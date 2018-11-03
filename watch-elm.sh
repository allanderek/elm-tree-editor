#!/bin/sh

# Note that you can pass argument here that will be relayed into 'compile-elm.sh' see the source for that
# for which arguments you can pass, in particular as I write this it is possible to pass in '--debug'.

find src -name "*.elm" | entr ./compile-elm.sh "$@"
