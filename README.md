# Advent of FPGA

* To generate verilog files: `./build.sh` or `dune exec build <one|seven> > output.v`
* To test: `dune runtest`

Hardcaml solutions to [day one](https://adventofcode.com/2025/day/1) and [day seven](https://adventofcode.com/2025/day/7)
of Advent of Code. Each module takes input in the form of ascii bytes in a uart-like interface and produces the output as
binary integers wrapped in a `With_valid`. Tests use the expect-test framework by printing the waveterm output and final
solution. It also cross-checks the hardware solution against a known good software implementation. The AoC author
doesn't want the input files to be redistributed so we only test against a small portion of the real input.

Some thoughts from this project as a noob at HW design and ocaml:

* Dune runtest is slow, even for incremental changes
* Opening modules locally to embed a dsl in the current scope is real neat (i.e. `let open Always in`)
* But name shadowing is annoying, what do you mean I can't use `width` as a variable name when I open Signal?
Are lib users constantly broken by new variable names in version upgrades? I guess it's poor style to open large modules.
* I didn't miss printf-style or regular debugging once I figured out how to dump vcd files.
* Day seven's verilog output is 17K lines long! I'm sure I've done something exceedingly dumb in the design.
* Claude is a lifesaver at deciphering ocaml compiler errors and module wrangling.