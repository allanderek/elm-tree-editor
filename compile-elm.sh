#!/bin/sh

#Black         0;30
#Dark Gray     1;30
#Red           0;31
#Light Red     1;31
#Green         0;32
#Light Green   1;32
#Brown/Orange  0;33
#Yellow        1;33
#Blue          0;34
#Light Blue    1;34
#Purple        0;35
#Light Purple  1;35
#Cyan          0;36
#Light Cyan    1;36
#Light Gray    0;37
#White         1;37

SEPARATORCOLOR='\033[0;35m'
NOCOLOR='\033[0m'

OUTPUTFILE=static/main.js

OPTORDEBUG="--optimize"
# idiomatic parameter and option handling in sh
while test $# -gt 0
do
    case "$1" in
        --debug) OPTORDEBUG="--debug"
            ;;
        --*) echo "Never heard of this option: $1"
            ;;
        *) echo "I don't need any arguments: $1"
            ;;
    esac
    shift
done



echo "${SEPARATORCOLOR}== Compiling Elm =========================================== ${OUTPUTFILE} ${NOCOLOR}"
elm make src/Main.elm --output=${OUTPUTFILE} $OPTORDEBUG
