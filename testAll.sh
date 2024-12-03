#!/bin/bash

OK=0

function displaytime {
  local T=$(($1/1000))
  local H=$((T/60/60))
  local M=$((T/60%60))
  local S=$((T%60))
  local MS=$(($1%1000))
  (( $H > 0 )) && printf '%dh ' $H
  (( $M > 0 || $H > 0 )) && printf '%dm ' $M
  (( $S > 0 || $M > 0 || $H > 0 )) && printf '%d.%03ds' $S $MS
  (( $H == 0 && $M == 0 && $S == 0 )) && printf '%dms\n' $MS
}

function printPuzzle {
  puzzle=$1
  printf "%14.14s: " $puzzle
  startTime=$(date +%s%0N)
  prolog -q -l $puzzle -t "verifyTests" || OK=1
  endTime=$(date +%s%0N)
  duration=$(( ($endTime-$startTime)/1000000 ))
  echo " ($(displaytime $duration))"  
}

function listPuzzles {
  if [ -z "$1" ]; then
    ls $1*.prolog | grep -v debug | grep -v common | grep -v \'
  else
    ls $1*.prolog | grep -v debug | grep -v common
  fi
}

IFS=$'\n'
for puzzle in $(listPuzzles "$1" | sort -V); do
  printPuzzle $puzzle
done

exit $OK
