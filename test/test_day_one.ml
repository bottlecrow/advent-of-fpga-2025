open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Day_one = Advent_of_fpga_2025.Day_one
module Sim = Cyclesim.With_interface (Day_one.I) (Day_one.O)

let test_input
  ~(here : [%call_pos])
  ?(create_vcd = false)
  ?(display_width = 100)
  ?(print_waveform = true)
  ?(cycles_per_baud = 1)
  input_str
  =
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let waves, sim =
    Waveform.create
      (Sim.create
         ~config:Cyclesim.Config.trace_all
         ~name:"day_one"
         (Day_one.create scope))
  in
  let sim, oc =
    if create_vcd
    then (
      let oc = Stdlib.Out_channel.open_text "/tmp/day_one.vcd" in
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
  String.iter adjusted_input ~f:(fun c ->
    let byte = Bits.of_char c in
    inputs.byte_in.valid := Bits.vdd;
    inputs.byte_in.value := byte;
    Cyclesim.cycle sim;
    inputs.byte_in.valid := Bits.gnd;
    Cyclesim.cycle sim ~n:(cycles_per_baud - 1);
    cycle_count := !cycle_count + cycles_per_baud);
  while not (Bits.equal !(outputs.valid) Bits.vdd) do
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  Cyclesim.cycle sim;
  Int.incr cycle_count;
  let byte_in_format =
    Wave_format.Custom
      (fun bits ->
        match Bits.to_int_trunc bits |> Char.of_int_exn with
        | '\n' -> "\\n"
        | c when Char.is_print c -> String.make 1 c
        | c -> sprintf "\\x%02x" (Char.to_int c))
  in
  let sm_format =
    let width = Bits.num_bits_to_represent (List.length Day_one.States.all) in
    Wave_format.Map
      (List.mapi Day_one.States.all ~f:(fun i state ->
         Bits.of_int_trunc ~width i, Day_one.States.to_string state))
  in
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob ~anchored:true ~expand_braces:true "{clock,dial,valid,value*}"
         |> Re.compile)
    ; Display_rule.port_name_is ~wave_format:Int "dial_turn"
    ; Display_rule.port_name_is ~wave_format:byte_in_format "byte_in$value"
    ; Display_rule.port_name_is ~wave_format:sm_format "sm"
    ]
  in
  if print_waveform
  then Waveform.print ~display_rules ~display_width ~signals_width:25 ~wave_width:1 waves;
  let reference = Day_one.reference_solution input_str in
  let part_one = Bits.to_unsigned_int !(outputs.value.part_one) in
  let part_two = Bits.to_unsigned_int !(outputs.value.part_two) in
  Option.iter oc ~f:Out_channel.close;

  [%test_eq: int] ~here:[ here ] part_one reference.part_one;
  [%test_eq: int] ~here:[ here ] part_two reference.part_two;
  print_s [%message (part_one : int) (part_two : int)]
;;

let%expect_test "never passing zero" =
  test_input {|
  L50
  R30
  L30
  L10
  R10
  L100
  |};
  [%expect
    {|
    ┌Signals────────────────┐┌Waves────────────────────────────────────────────────────────────────────┐
    │clock                  ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌│
    │                       ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘│
    │valid                  ││                                                                         │
    │                       ││─────────────────────────────────────────────────────────────────────────│
    │                       ││────────────────┬───────────────────────────────┬────────────────────────│
    │value$part_one         ││ 0              │1                              │2                       │
    │                       ││────────────────┴───────────────────────────────┴────────────────────────│
    │                       ││────────────────┬───────────────────────────────┬────────────────────────│
    │value$part_two         ││ 0              │1                              │2                       │
    │                       ││────────────────┴───────────────────────────────┴────────────────────────│
    │                       ││────────────────┬───────────────┬───────────────┬───────────────┬────────│
    │dial                   ││ 50             │0              │30             │0              │90      │
    │                       ││────────────────┴───────────────┴───────────────┴───────────────┴────────│
    │                       ││────────┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬│
    │dial_turn              ││ 0      │-5 │-50    │0  │3  │30     │0  │-3 │-30    │0  │-1 │-10    │0  ││
    │                       ││────────┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴│
    │                       ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬│
    │byte_in$value          ││ L  │5  │0  │\n │R  │3  │0  │\n │L  │3  │0  │\n │L  │1  │0  │\n │R  │1  ││
    │                       ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴│
    │                       ││────┬───────────┬───┬───────────┬───┬───────────┬───┬───────────┬───┬────│
    │sm                     ││ Re.│Read_ticks │Re.│Read_ticks │Re.│Read_ticks │Re.│Read_ticks │Re.│Read│
    │                       ││────┴───────────┴───┴───────────┴───┴───────────┴───┴───────────┴───┴────│
    └───────────────────────┘└─────────────────────────────────────────────────────────────────────────┘
    ((part_one 4) (part_two 4))
    |}]
;;

let%expect_test "always passing zero" =
  test_input {|
  L60
  R20
  L80
  R180
  |};
  [%expect
    {|
    ┌Signals────────────────┐┌Waves────────────────────────────────────────────────────────────────────┐
    │clock                  ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌│
    │                       ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘│
    │valid                  ││                                                                        ┌│
    │                       ││────────────────────────────────────────────────────────────────────────┘│
    │                       ││─────────────────────────────────────────────────────────────────────────│
    │value$part_one         ││ 0                                                                       │
    │                       ││─────────────────────────────────────────────────────────────────────────│
    │                       ││────────────────┬───────────────┬───────────────┬───────────────────┬────│
    │value$part_two         ││ 0              │1              │2              │3                  │5   │
    │                       ││────────────────┴───────────────┴───────────────┴───────────────────┴────│
    │                       ││────────────────┬───────────────┬───────────────┬───────────────────┬────│
    │dial                   ││ 50             │90             │10             │30                 │10  │
    │                       ││────────────────┴───────────────┴───────────────┴───────────────────┴────│
    │                       ││────────┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬───┬───┬────────│
    │dial_turn              ││ 0      │-6 │-60    │0  │2  │20     │0  │-8 │-80    │0  │1  │18 │180     │
    │                       ││────────┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴───┴───┴────────│
    │                       ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬────────│
    │byte_in$value          ││ L  │6  │0  │\n │R  │2  │0  │\n │L  │8  │0  │\n │R  │1  │8  │0  │\n      │
    │                       ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴────────│
    │                       ││────┬───────────┬───┬───────────┬───┬───────────┬───┬───────────────┬───┬│
    │sm                     ││ Re.│Read_ticks │Re.│Read_ticks │Re.│Read_ticks │Re.│Read_ticks     │Re.││
    │                       ││────┴───────────┴───┴───────────┴───┴───────────┴───┴───────────────┴───┴│
    └───────────────────────┘└─────────────────────────────────────────────────────────────────────────┘
    ((part_one 0) (part_two 5))
    |}]
;;

let%expect_test "small case" =
  test_input
    ~display_width:90
    {|
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
  |};
  [%expect
    {|
    ┌Signals────────────────┐┌Waves──────────────────────────────────────────────────────────┐
    │clock                  ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │                       ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │valid                  ││                                                               │
    │                       ││───────────────────────────────────────────────────────────────│
    │                       ││────────────────────────────────────────────────┬──────────────│
    │value$part_one         ││ 0                                              │1             │
    │                       ││────────────────────────────────────────────────┴──────────────│
    │                       ││────────────────┬───────────────────────────────┬──────────────│
    │value$part_two         ││ 0              │1                              │2             │
    │                       ││────────────────┴───────────────────────────────┴──────────────│
    │                       ││────────────────┬───────────────┬───────────────┬───────────┬──│
    │dial                   ││ 50             │82             │52             │0          │95│
    │                       ││────────────────┴───────────────┴───────────────┴───────────┴──│
    │                       ││────────┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬──────│
    │dial_turn              ││ 0      │-6 │-68    │0  │-3 │-30    │0  │4  │48     │0  │-5    │
    │                       ││────────┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴──────│
    │                       ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬──│
    │byte_in$value          ││ L  │6  │8  │\n │L  │3  │0  │\n │R  │4  │8  │\n │L  │5  │\n │R │
    │                       ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴──│
    │                       ││────┬───────────┬───┬───────────┬───┬───────────┬───┬───────┬──│
    │sm                     ││ Re.│Read_ticks │Re.│Read_ticks │Re.│Read_ticks │Re.│Read_t.│Re│
    │                       ││────┴───────────┴───┴───────────┴───┴───────────┴───┴───────┴──│
    └───────────────────────┘└───────────────────────────────────────────────────────────────┘
    ((part_one 3) (part_two 6))
    |}]
;;

let%expect_test "first 100 lines of official day one input" =
  let input = In_channel.read_all "day_one_small.input" in
  test_input ~print_waveform:false input;
  [%expect {| ((part_one 11) (part_two 42)) |}]
;;
