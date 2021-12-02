# AOC 2021

This repo contains the solutions I used to solve the [Advent Of Code](https://adventofcode.com/2021) puzzles.

### Layout

Each day gets a executable target in cabal of the form `aoc<day>`. For each particular day, the main file is `Main<day>.hs`.

For example, day 1's executable would be called `aoc1` with `Main1.hs` as the main file name.

For convenience, `input.txt` is gitignore'd to store your input to test.

### Building

To build a specific day's solution:

```sh
cabal build aoc1 
```

To build all days at once:

```sh
cabal build
```

### Running

All executables require two arguments to them. The first argument is the puzzle number within that day to solve and the second is an input file containing the input.

For example, to solve the 1st problem of the 2nd day where the input is stored in `input.txt`, run:

```sh
cabal run aoc2 -- 1 input.txt
```
