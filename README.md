# Advent of FPGA

* To generate verilog files: `./build.sh` or `dune exec build <one|seven|eleven> > output.v`
* To test: `dune runtest`

Hardcaml solutions to [day one](https://adventofcode.com/2025/day/1), [day seven](https://adventofcode.com/2025/day/7), and [day eleven](https://adventofcode.com/2025/day/11) of Advent of Code. Each module takes input in the form of ascii bytes in a uart-like interface and produces the output as binary integers wrapped in a `With_valid`. Tests use the expect-test framework by printing the waveterm output and final solution. It also cross-checks the hardware solution against a known good software implementation. The AoC author doesn't want the input files to be redistributed so we only test against a small portion of the real input or make our own.

## Day one
We track the dial position (0-99) in a register, updating it after each turn by adding or subtracting ticks based on direction. Part one increments a counter when the dial lands on zero. Part two counts zero-crossings by computing how many times we pass zero during a turn: we add the ticks to our distance from the nearest zero (in the turn direction) and divide by 100. We fix up the quotient from truncating division by adding 1 if negative.

## Day seven
The first line marks the beam start position 'S', initializing that column's strength to 1. For each subsequent row, we record splitter positions in splitter_row. On newline, we update beam strengths in one cycle: each non-splitter cell receives its current value plus contributions from left/right neighbors that have splitters. Part one counts splits (non-zero right contributions). Part two sums final beam strengths.

## Day eleven
Init_graph populates three arrays: "names" maps base-26 encoded node names to sequential node IDs (starting from 1), "nodes" maps each node ID to its edge range (start index inclusively and end index exclusively) in the "edges" array, which stores child node IDs.

Count_paths performs DFS using a hardware stack. Each stack entry tracks the node ID and a 2-bit count of how many key nodes ("dac", "fft") were seen on that path. Part one increments when reaching "out". Part two increments when reaching "out" with seen count equal to 2. Because the graph is acyclic, a count of 2 means both key nodes were visited exactly once.


## Some thoughts from this project as a noob at HW design and ocaml

* Dune runtest is slow, even for incremental changes
* Opening modules locally to embed a dsl in the current scope is real neat (i.e. `let open Always in`)
* But name shadowing is annoying, what do you mean I can't use `width` as a variable name when I open Signal locally?
Are lib users constantly broken by new variable names in version upgrades? I guess it's poor style to open large modules.
* I didn't miss printf-style or regular debugging once I figured out how to dump vcd files.
* Day seven's verilog output is 17K lines long! I'm sure I've done something exceedingly dumb in the design.
* Claude is a lifesaver at deciphering ocaml compiler errors and module wrangling.