<!-- Automatically generated from README.md.gyb, do not edit directly! -->

# Advent of Code 2023

My solutions to the [Advent of Code 2023](https://adventofcode.com/2023), written in 25 different programming languages.


## Scripts

Each day includes two scripts:

- `./bootstrap` installs the language (compiler or interpreter) and project dependencies if needed
- `./run` builds and runs the program

Some days that need additional configuration also have environment-related scripts invoked by CI:

- `./path` computes a list of entries to dynamically append to the `PATH`
- `./env` computes a list of environment variables to set

This standardized pattern lets CI use a single workflow (per OS) across all days. Additionally, they make it easy to get started developing locally even across the range of different languages, build tools and package managers involved.

> Note that some bootstrap scripts are still geared around CI use, so you may still prefer to install the corresponding toolchain using your package manager manually.

## Previous years

My solutions to the previous challenges can be found here:

- [`advent-of-code-2022`](https://github.com/fwcd/advent-of-code-2022)
- [`advent-of-code-2021`](https://github.com/fwcd/advent-of-code-2021)
- [`advent-of-code-2020`](https://github.com/fwcd/advent-of-code-2020)
- [`advent-of-code-2019`](https://github.com/fwcd/advent-of-code-2019)
- [`advent-of-code-2015`](https://github.com/fwcd/advent-of-code-2015)
