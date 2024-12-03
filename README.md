# Advent of Code

This repository contains my attemps to solve the puzzles of [Advent of Code 2024](https://adventofcode.com/2024). I will try to keep a naming scheme like `[day]_[part].prolog` so e.g. `1_1.prolog` will be the first puzzle of the first day. Please be fair and do not look at solutions to riddles you have not solved yourself.

## Execution

You can either load and execute each riddle individually in prolog by running the goal `solve.` or just execute the bash script `solveAll.sh` which will solve all puzzles in the repository in ascending order. `solveAll.sh` accepts an optional argument that is used as a prefix to filter the files that shall be executes. So e.g. `./solveAll.sh 2_` will only solve the puzzles of day 2.

Either way you will need a prolog interpreter (e.g. swi-prolog) - which needs to be accessible on the path as  `prolog` for the bash script to work.

## Choice of Programming Language

I try to solve all of the puzzles in Prolog, although I have almost no experience in Prolog at all. I only had one introduction into Prolog ~20 years ago and started to use it again for solving all AoC puzzles of 2020. I kind of liked it to work with something different once a year so I sticked to Prolog for the 2021, 2022 and 2023 riddles and will again try again this year. But anybody who has experience in Prolog will probably bang his head against the wall when he sees my solutions. ;)