# Advent of Code

This repository contains my attemps to solve the puzzles of [Advent of Code 2024](https://adventofcode.com/2024). I will try to keep a naming scheme like `day[_part].prolog` where the `_part` is optional. If the filename contains a part it will only contain the solution to this part of the day. Filenames without the part will contain both parts. So e.g. `1_1.prolog` will be the first puzzle of the first day, but e.g. `2.prolog` will contain both parts for the second day. Please be fair and do not look at solutions to riddles you have not solved yourself.

## Execution

You can either load and execute each riddle individually in prolog by running the goal `solve.` or just execute the bash script `solveAll.sh` which will solve all puzzles in the repository in ascending order. `solveAll.sh` accepts two optional arguments that select which day and part shall be executed. So e.g. `./solveAll.sh 2` will only solve the puzzles of day 2 and e.g. `./solveAll.sh 3 1` will only solve the first part of day 3. The part argument will be ignored if the day is set to an empty string.

There is also a script `testAll.sh` that accepts the same parameters as `solveAll.sh` but only executes the tests for the selected puzzles.

Either way you will need a prolog interpreter (e.g. swi-prolog) - which needs to be accessible on the path as  `prolog` for the bash script to work.

## Choice of Programming Language

I try to solve all of the puzzles in Prolog, although I have almost no experience in Prolog at all. I only had one introduction into Prolog ~20 years ago and started to use it again for solving all AoC puzzles of 2020. I kind of liked it to work with something different once a year so I sticked to Prolog for the 2021, 2022 and 2023 riddles and will again try again this year. But anybody who has experience in Prolog will probably bang his head against the wall when he sees my solutions. ;)