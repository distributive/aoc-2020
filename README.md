# AoC 2020

These are my solutions for Advent of Code 2020 https://adventofcode.com/, built on Sam Coy's Haskell template: https://github.com/samcoy3/advent-of-code-2020/

Below are the usage instructions as per his original documentation. Please see the original repository for full documentation on the template.

When running from the command line you can pass the option `-d/--day DAY` to run a specific day's solutions. If you do this, then you can also pass `-i/--input FILE` to specify an input file; by default, the program will look for it in `input/DayXX.txt`. You can also pass the argument `--all-days` and all days will be run in order, assuming the input files are in their default places.

Example usage:
- `stack run -- -d 9`: Runs Day 9's solutions.
- `stack run -- --day 14 --input "wibble.txt"`: Runs Day 14's solutions, using the input file "wibble.txt".
- `stack run -- --all-days`: Runs the solutions from all days.
