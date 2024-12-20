#!/bin/bash

OK=0

function printPuzzle {
  puzzle=$1
  if [[ -z "$2" ]]; then
    parts=$(prolog -q -l $puzzle -t "parts")
  else
    parts="part$2"
  fi
  for part in $parts; do
    local fullPuzzle result
    result=1
    if [[ "$part" != "single" ]]; then
      fullPuzzle="$(echo "$puzzle" | sed -n 's/\.prolog$//p')/$(echo "$part" | sed -n 's/^part//p')"
    else
      fullPuzzle="$(echo "$puzzle" | sed -n 's/\.prolog$//p')"
    fi
    printf "%6.6s: " $fullPuzzle
    prolog -q -l $puzzle -t "verifyTests($part)" || result=0
    echo
    if [[ $result == 0 ]]; then
      OK=1
    fi
  done
}

IFS=$'\n'
if [ -z "$1" ]; then
  for puzzle in $(ls *.prolog 2> /dev/null | grep -v debug | grep -v common | grep -v \' | sort -V); do
    printPuzzle "$puzzle"
  done
else
  for puzzle in $(ls "$1.prolog" 2> /dev/null | grep -v debug | grep -v common); do
    printPuzzle "$puzzle" "$2"
  done
  for puzzle in $(ls "$1_$2"*.prolog 2> /dev/null | grep -v debug | grep -v common); do
    printPuzzle "$puzzle"
  done
fi
exit $OK
