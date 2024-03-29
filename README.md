<!-- Automatically generated from README.md.gyb, do not edit directly! -->

# Advent of Code 2023

[![Run](https://github.com/fwcd/advent-of-code-2023/actions/workflows/run.yml/badge.svg)](https://github.com/fwcd/advent-of-code-2023/actions/workflows/run.yml)

My solutions to the [Advent of Code 2023](https://adventofcode.com/2023), written in 25 different programming languages.

- [x] [**Day 01**](day01): [C](day01/src/day01.c)
- [x] [**Day 02**](day02): [COBOL](day02/src/day02.cob)
- [x] [**Day 03**](day03): [ALGOL 60](day03/src/day03.alg)
- [x] [**Day 04**](day04): [Nix](day04/src/day04.nix)
- [x] [**Day 05**](day05): [B](day05/src/day05.b) ([readme](day05/README.md))
- [x] [**Day 06**](day06): [LLVM IR](day06/src/day06.ll)
- [x] [**Day 07**](day07): [Prolog](day07/src/day07.pl)
- [x] [**Day 08**](day08): [Objective-C](day08/src/day08.m)
- [x] [**Day 09**](day09): [Curry](day09/src/Day09.curry)
- [x] [**Day 10**](day10): [Java](day10/src/Day10.java)
- [x] [**Day 11**](day11): [Crystal](day11/src/day11.cr)
- [x] [**Day 12**](day12): [MoonScript](day12/src/day12.moon)
- [x] [**Day 13**](day13): [Vala](day13/src/day13.vala)
- [x] [**Day 14**](day14): [Go](day14/src/day14.go)
- [x] [**Day 15**](day15): [Brainfuck](day15/src/part1.bf), [D](day15/src/part2.d)
- [x] [**Day 16**](day16): [Zig](day16/src/day16.zig)
- [x] [**Day 17**](day17): [PHP](day17/src/day17.php)
- [x] [**Day 18**](day18): [C++](day18/src/day18.cpp)
- [x] [**Day 19**](day19): [Swift](day19/src/day19.swift)
- [x] [**Day 20**](day20): [Kotlin/Native](day20/src/day20.kt)
- [x] [**Day 21**](day21): [C#](day21/src/day21.cs)
- [x] [**Day 22**](day22): [Rust](day22/src/day22.rs)
- [x] [**Day 23**](day23): [Ruby](day23/src/day23.rb)
- [x] [**Day 24**](day24): [Haskell](day24/src/Day24.hs)
- [x] [**Day 25**](day25): [Python](day25/src/day25.py)

## Development

The programs are packaged with [Nix](https://nixos.org/), a functional package manager for Linux and macOS that focuses on reproducible builds. This makes it easy to build the programs, both locally and CI, without relying on system packages.

To build one of the days, `cd` into the corresponding directory and build and/or run the Nix flake. For example, to run day 4, use the following commands:

```sh
cd day04
nix run . resources/input.txt
```

Every day is packaged up to take exactly one command-line argument, the input file, and usually includes the demo input from the exercise too.

> [!TIP]
> The build environment can be added to the current `PATH` using `nix develop`. This is useful to manually run the compiler.

## Lessons Learned

- Visualize the input with GraphViz (day [8](day08), [20](day20), [23](day23), [25](day25))
- Some puzzles are actually reverse-engineering exercises and rely on undocumented input constraints to be solved efficiently or even feasibly at all (day [8](day08), [20](day20), [23](day23))
- Take the [LCM](https://en.wikipedia.org/wiki/Least_common_multiple) to solve cycle alignment problems (day [8](day08), [20](day20))
  - If there are offsets, use the [CRT](https://en.wikipedia.org/wiki/Chinese_remainder_theorem) ([like in previous years](https://github.com/fwcd/advent-of-code-2020/blob/18c3ba9820cb52627366a632ccaab233a6d9f563/day13/src/day13.c#L39-L59))
- Binary counters can elegantly be modeled as chains of flip flop (day [20](day20))
- Cross products can be surprisingly useful to turn the most nonlinear-looking problems into linear equations (day [24](day24))

## Previous Years

My solutions to the previous challenges can be found here:

- [Advent of Code 2022](https://github.com/fwcd/advent-of-code-2022)
- [Advent of Code 2021](https://github.com/fwcd/advent-of-code-2021)
- [Advent of Code 2020](https://github.com/fwcd/advent-of-code-2020)
- [Advent of Code 2019](https://github.com/fwcd/advent-of-code-2019)
- [Advent of Code 2015](https://github.com/fwcd/advent-of-code-2015)
