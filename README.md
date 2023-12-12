<!-- Automatically generated from README.md.gyb, do not edit directly! -->

# Advent of Code 2023

[![Run](https://github.com/fwcd/advent-of-code-2023/actions/workflows/run.yml/badge.svg)](https://github.com/fwcd/advent-of-code-2023/actions/workflows/run.yml)

My solutions to the [Advent of Code 2023](https://adventofcode.com/2023), written in 25 different programming languages.

- [x] Day 01: [C](day01/src/day01.c)
- [x] Day 02: [COBOL](day02/src/day02.cob)
- [x] Day 03: [ALGOL 60](day03/src/day03.alg)
- [x] Day 04: [Nix](day04/src/day04.nix)
- [x] Day 05: [B](day05/src/day05.b) ([readme](day05/README.md))
- [x] Day 06: [LLVM IR](day06/src/day06.ll)
- [x] Day 07: [Prolog](day07/src/day07.pl)
- [x] Day 08: [Objective-C](day08/src/day08.m)
- [x] Day 09: [Curry](day09/src/Day09.curry)
- [x] Day 10: [Java](day10/src/Day10.java)
- [x] Day 11: [Crystal](day11/src/day11.cr)
- [ ] Day 12: [MoonScript](day12/src/day12.moon)

## Packaging

The programs are packaged with [Nix](https://nixos.org/), a functional package manager for Linux and macOS that focuses on reproducible builds. This makes it easy to build the programs, both locally and CI, without relying on system packages.

To build one of the days, `cd` into the corresponding directory and `nix build` the flake. To run it, use `nix run . <path to input>`. For example:

```sh
cd day04
nix run . resources/input.txt
```

> [!NOTE]
> Nix will automatically create a symlink pointing to the built derivation in the Nix store, from where the binaries can also be executed manually.

Every day is packaged up to take exactly one command-line argument, the input file, and usually includes the demo input from the exercise too.

## Previous years

My solutions to the previous challenges can be found here:

- [Advent of Code 2022](https://github.com/fwcd/advent-of-code-2022)
- [Advent of Code 2021](https://github.com/fwcd/advent-of-code-2021)
- [Advent of Code 2020](https://github.com/fwcd/advent-of-code-2020)
- [Advent of Code 2019](https://github.com/fwcd/advent-of-code-2019)
- [Advent of Code 2015](https://github.com/fwcd/advent-of-code-2015)
