open! Core
open! Hardcaml
open! Signal
module Day_one = Advent_of_fpga_2025.Day_one
module Circuit_one = Circuit.With_interface (Day_one.I) (Day_one.O)
module Day_seven = Advent_of_fpga_2025.Day_seven
module Circuit_seven = Circuit.With_interface (Day_seven.I) (Day_seven.O)
module Day_eleven = Advent_of_fpga_2025.Day_eleven
module Circuit_eleven = Circuit.With_interface (Day_eleven.I) (Day_eleven.O)

let write_verilog ~create =
  let scope = Scope.create ~flatten_design:false () in
  let circuit = create scope in
  let database = Scope.circuit_database scope in
  Rtl.print ~database Verilog circuit
;;

let usage () =
  Printf.eprintf "Usage: dune exec build [one|seven] > output.v\n";
  exit 1
;;

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then usage ();
  match argv.(1) with
  | "one" ->
    write_verilog ~create:(fun scope ->
      Circuit_one.create_exn ~name:"day_one" (Day_one.create scope))
  | "seven" ->
    write_verilog ~create:(fun scope ->
      Circuit_seven.create_exn ~name:"day_seven" (Day_seven.create scope))
  | "eleven" ->
    write_verilog ~create:(fun scope ->
      Circuit_eleven.create_exn ~name:"day_eleven" (Day_eleven.create scope))
  | _ -> usage ()
;;
