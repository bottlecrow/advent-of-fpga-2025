open! Hardcaml
open! Hardcaml_waveterm
module Day_eleven = Advent_of_fpga_2025.Day_eleven
module Sim = Cyclesim.With_interface (Day_eleven.I) (Day_eleven.O)
open! Core

let%expect_test "example input part one" =
  let { Day_eleven.part_one; _ } =
    Day_eleven.reference_solution
      {|
      aaa: you hhh
      you: bbb ccc
      bbb: ddd eee
      ccc: ddd eee fff
      ddd: ggg
      eee: out
      fff: out
      ggg: out
      hhh: ccc fff iii
      iii: out
  |}
  in
  print_s [%message (part_one : int)];
  [%expect {| (part_one 5) |}]
;;

let%expect_test "example input part two" =
  let { Day_eleven.part_two; _ } =
    Day_eleven.reference_solution
      {|
      svr: aaa bbb
      aaa: fft
      fft: ccc
      bbb: tty
      tty: ccc
      ccc: ddd eee
      ddd: hub
      hub: fff
      eee: dac
      dac: fff
      fff: ggg hhh
      ggg: out
      hhh: out
  |}
  in
  print_s [%message (part_two : int)];
  [%expect {| (part_two 2) |}]
;;

let test_phase
  ~(_here : [%call_pos])
  ?(create_vcd = false)
  ?(display_width = 100)
  ?(print_waveform = false)
  ?(cycles_per_baud = 4)
  input_str
  (phase : Day_eleven.States.t)
  =
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let waves, sim =
    Waveform.create
      (Sim.create
         ~config:Cyclesim.Config.trace_all
         ~name:"day_eleven"
         (Day_eleven.create scope))
  in
  let sim, oc =
    if create_vcd
    then (
      let oc = Stdlib.Out_channel.open_text "/tmp/day_eleven.vcd" in
      let sim = Vcd.wrap oc sim in
      sim, Some oc)
    else sim, None
  in
  let outputs = Cyclesim.outputs sim in
  let inputs = Cyclesim.inputs sim in
  let adjusted_input =
    (input_str
     |> String.split_lines
     |> List.map ~f:String.strip
     |> List.filter ~f:(Fn.non String.is_empty)
     |> String.concat ~sep:"\n")
    ^ "\n\n"
  in
  let cycle_count = ref 0 in
  let get_current_state () =
    List.nth_exn Day_eleven.States.all (Bits.to_unsigned_int !(outputs.value.state))
  in
  String.iter adjusted_input ~f:(fun c ->
    inputs.byte_in.valid := Bits.gnd;
    Cyclesim.cycle sim ~n:(cycles_per_baud - 1);
    inputs.byte_in.valid := Bits.vdd;
    inputs.byte_in.value := Bits.of_char c;
    Cyclesim.cycle sim;
    cycle_count := !cycle_count + cycles_per_baud);
  let target_state =
    List.nth_exn Day_eleven.States.all (Day_eleven.States.Variants.to_rank phase + 1)
  in
  while Poly.(get_current_state () <> target_state) do
    inputs.byte_in.valid := Bits.gnd;
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  Cyclesim.cycle sim;
  Int.incr cycle_count;
  if print_waveform
  then Waveform.print ~display_width ~signals_width:25 ~wave_width:1 waves;
  Option.iter oc ~f:Out_channel.close;
  !cycle_count, sim
;;

let read_memory sim name =
  Cyclesim.lookup_mem_by_name sim name
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
  |> Array.map ~f:Bits.to_int_trunc
;;

let print_nodes sim input_str =
  let unique_names =
    String.split_on_chars input_str ~on:[ '\n'; ':'; ' ' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.Set.of_list
    |> Set.to_list
  in
  let names = read_memory sim "names" in
  let nodes = read_memory sim "nodes" in
  let edges = read_memory sim "edges" in
  (* printf !"nodes=%{sexp:int array}\n" @@ Array.sub nodes ~pos:0 ~len:30; *)
  (* printf !"edges=%{sexp:int array}\n" @@ Array.sub edges ~pos:0 ~len:30; *)
  let name_index name =
    String.to_sequence name
    |> Sequence.fold ~init:0 ~f:(fun acc c ->
      (acc * 26) + (Char.to_int c - Char.to_int 'a'))
  in
  (* List.iter unique_names ~f:(fun name ->
    let index = name_index name in
    let node_idx = names.(index) - 1 in
    printf !"%s -> x%x -> %d\n" name index node_idx); *)
  List.iter unique_names ~f:(fun name ->
    let index = name_index name in
    let node_idx = names.(index) - 1 in
    let edges_start = nodes.(node_idx * 2) in
    let edges_end = nodes.((node_idx * 2) + 1) in
    let edges = Array.sub edges ~pos:edges_start ~len:(edges_end - edges_start) in
    printf !"%s[%d] edges=%{sexp:int array}\n" name (node_idx + 1) edges)
;;

let%expect_test "init small graph " =
  let input = {|
  aaa: bbb ccc
  bbb: ddd eee
  eee: fff
  |} in
  let _, sim = test_phase input Day_eleven.States.Init_graph in
  print_nodes sim input;
  [%expect
    {|
    aaa[1] edges=(2 3)
    bbb[2] edges=(4 5)
    ccc[3] edges=(6)
    ddd[4] edges=()
    eee[5] edges=()
    fff[6] edges=()
    |}]
;;

let%expect_test "init example graph" =
  let input =
    {|
      svr: aaa bbb
      aaa: fft
      fft: ccc
      bbb: tty
      tty: ccc
      ccc: ddd eee
      ddd: hub
      hub: fff
      eee: dac
      dac: fff
      fff: ggg hhh
      ggg: out
      hhh: out
  |}
  in
  let _, sim = test_phase ~create_vcd:true input Day_eleven.States.Init_graph in
  print_nodes sim input;
  [%expect
    {|
    aaa[2] edges=(4)
    bbb[3] edges=(5)
    ccc[5] edges=(5)
    dac[11] edges=(12 13)
    ddd[7] edges=(9)
    eee[8] edges=(10)
    fff[10] edges=(10)
    fft[4] edges=(6)
    ggg[12] edges=(14)
    hhh[13] edges=(14)
    hub[9] edges=(11)
    out[14] edges=()
    svr[1] edges=(2 3)
    tty[6] edges=(7 8)
    |}]
;;
