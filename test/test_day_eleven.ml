open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Day_eleven = Advent_of_fpga_2025.Day_eleven
module Sim = Cyclesim.With_interface (Day_eleven.I) (Day_eleven.O)

let%expect_test "example input part one" =
  let { Day_eleven.part_one; _ } =
    Day_eleven.reference_solution
      {|
      aaa: svr hhh
      svr: bbb ccc
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

module type Circuit = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t
end

module Test_harness (C : Circuit) = struct
  module Sim = Cyclesim.With_interface (C.I) (C.O)

  let run ?(create_vcd = false) ?(display_width = 100) ?(print_waveform = false) ~name () =
    let scope =
      Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()
    in
    let waves, sim =
      Waveform.create
        (Sim.create ~config:Cyclesim.Config.trace_all ~name (C.create scope))
    in
    let sim, oc =
      if create_vcd
      then (
        let oc = Stdlib.Out_channel.open_text ("/tmp/" ^ name ^ ".vcd") in
        let sim = Vcd.wrap oc sim in
        sim, Some oc)
      else sim, None
    in
    let cleanup () =
      if print_waveform
      then Waveform.print ~display_width ~signals_width:25 ~wave_width:1 waves;
      Option.iter oc ~f:Out_channel.close
    in
    sim, cleanup
  ;;
end

let test_phase
  ~(_here : [%call_pos])
  ?(create_vcd = false)
  ?(display_width = 100)
  ?(print_waveform = false)
  ?(cycles_per_baud = 4)
  input_str
  (phase : Day_eleven.States.t)
  =
  let module T = Test_harness (Day_eleven) in
  let sim, cleanup =
    T.run ~create_vcd ~display_width ~print_waveform ~name:"day_eleven" ()
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
  let continue_condition =
    match
      List.nth Day_eleven.States.all (Day_eleven.States.Variants.to_rank phase + 1)
    with
    | Some target_state -> fun () -> Poly.(get_current_state () <> target_state)
    | None -> fun () -> not (Bits.to_bool !(outputs.valid))
  in
  while continue_condition () do
    inputs.byte_in.valid := Bits.gnd;
    Cyclesim.cycle sim;
    Int.incr cycle_count
  done;
  Cyclesim.cycle sim;
  Int.incr cycle_count;
  cleanup ();
  !cycle_count, sim
;;

let read_memory sim name =
  Cyclesim.lookup_mem_by_name sim name
  |> Option.value_exn
  |> Cyclesim.Memory.read_all
  |> Array.map ~f:Bits.to_int_trunc
;;

let print_nodes ?(verbose = false) sim input_str =
  let unique_names =
    String.split_on_chars input_str ~on:[ '\n'; ':'; ' ' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.Set.of_list
    |> Set.to_list
  in
  let names = read_memory sim "names" in
  let nodes = read_memory sim "nodes" in
  let edges = read_memory sim "edges" in
  if verbose
  then (
    printf !"nodes=%{sexp:(int * int * int) list}\n"
    @@ List.init 15 ~f:(fun i -> i + 1, nodes.(i * 2), nodes.((i * 2) + 1));
    printf !"edges=%{sexp:int array}\n" @@ Array.sub edges ~pos:0 ~len:30;
    List.iter unique_names ~f:(fun name ->
      let index = Day_eleven.name_index name in
      let node_id = names.(index) in
      printf !"%s -> x%x -> %d\n" name index node_id));
  List.iter unique_names ~f:(fun name ->
    let index = Day_eleven.name_index name in
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
    ccc[3] edges=()
    ddd[4] edges=()
    eee[5] edges=(6)
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
  let _, sim = test_phase input Day_eleven.States.Init_graph in
  print_nodes sim input;
  [%expect
    {|
    aaa[2] edges=(4)
    bbb[3] edges=(6)
    ccc[5] edges=(7 8)
    dac[11] edges=(10)
    ddd[7] edges=(9)
    eee[8] edges=(11)
    fff[10] edges=(12 13)
    fft[4] edges=(5)
    ggg[12] edges=(14)
    hhh[13] edges=(14)
    hub[9] edges=(10)
    out[14] edges=()
    svr[1] edges=(2 3)
    tty[6] edges=(5)
    |}]
;;

let%expect_test "dfs stack" =
  let test_inputs =
    [ [ "push", 1, "00"; "push", 2, "01"; "push", 3, "10"; "push", 4, "11" ]
    ; [ "push", 5, "11"
      ; "push", 6, "01"
      ; "push", 7, "00"
      ; "push", 8, "01"
      ; "pop", 0, ""
      ; "pop", 0, "0"
      ]
    ]
  in
  let module T = Test_harness (Day_eleven.Dfs_stack) in
  let sim, cleanup = T.run ~name:"dfs_stack" () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let test_input i =
    let ops =
      List.map i ~f:(fun (op, node_id, seen) ->
        (match op with
         | "push" ->
           inputs.pop := Bits.gnd;
           inputs.push.valid := Bits.vdd;
           inputs.push.value.node_id
           := Bits.of_int_trunc ~width:Day_eleven.node_id_width node_id;
           inputs.push.value.seen := Bits.of_string seen;
           Cyclesim.cycle sim
         | "pop" ->
           inputs.push.valid := Bits.gnd;
           inputs.pop := Bits.vdd;
           Cyclesim.cycle sim
         | _ -> failwithf !"Unknown operation: %s" op ());
        let top_value =
          if Bits.to_bool !(outputs.top.valid)
          then Int.to_string (Bits.to_int_trunc !(outputs.top.value.node_id))
          else "(empty)"
        in
        let size = Bits.to_int_trunc !(outputs.size) in
        sprintf "%s|%d" top_value size)
    in
    printf !"%s\n" (String.concat ~sep:", " ops);
    let mem = read_memory sim "stack_mem" in
    printf !"mem=%{sexp:int array}\n" @@ Array.sub mem ~pos:0 ~len:20;
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd
  in
  List.iter test_inputs ~f:test_input;
  cleanup ();
  [%expect
    {|
    1|1, 2|2, 3|3, 4|4
    mem=(0 1 2 3 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    5|1, 6|2, 7|3, 8|4, 7|3, 6|2
    mem=(0 5 6 7 8 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    |}]
;;

let test_count_paths ?(create_vcd = false) input =
  let reference = Day_eleven.reference_solution input in
  let _, sim = test_phase ~create_vcd input Day_eleven.States.End in
  let outputs = Cyclesim.outputs sim in
  let part_one = Bits.to_int_trunc !(outputs.value.part_one) in
  let part_two = Bits.to_int_trunc !(outputs.value.part_two) in

  [%test_eq: int] part_one reference.part_one;
  [%test_eq: int] part_two reference.part_two;
  print_s [%message (part_one : int) (part_two : int)]
;;

let%expect_test "count paths small graph" =
  let input = {|
  svr: dac ccc
  dac: ddd fft
  fft: out
  |} in
  test_count_paths input;
  [%expect {| ((part_one 1) (part_two 1)) |}]
;;

let%expect_test "official example input" =
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
  test_count_paths input;
  [%expect {| ((part_one 8) (part_two 2)) |}]
;;

let create_random_graph ~seed ~max_fanout ~depth =
  let state = Random.State.make [| seed |] in
  let rand_int n = Random.State.int state n in
  let random_name () =
    let c () = Char.of_int_exn (Char.to_int 'a' + rand_int 26) in
    sprintf "%c%c%c" (c ()) (c ()) (c ())
  in
  let reserved = Set.of_list (module String) [ "svr"; "dac"; "fft"; "out" ] in
  let rec random_unique_name used =
    let name = random_name () in
    if Set.mem reserved name || Set.mem used name then random_unique_name used else name
  in
  assert (depth >= 4);
  let levels = Array.init depth ~f:(fun _ -> []) in
  levels.(0) <- [ "svr" ];
  levels.(depth - 1) <- [ "out" ];
  let dac_level = 1 + rand_int (depth - 2) in
  let fft_level =
    let rec pick () =
      let l = 1 + rand_int (depth - 2) in
      if l = dac_level then pick () else l
    in
    pick ()
  in
  let used = ref reserved in
  for level = 1 to depth - 2 do
    let special =
      (if level = dac_level then [ "dac" ] else [])
      @ if level = fft_level then [ "fft" ] else []
    in
    let num_extra =
      if List.is_empty special then 1 + rand_int max_fanout else rand_int max_fanout
    in
    let extra =
      List.init num_extra ~f:(fun _ ->
        let name = random_unique_name !used in
        used := Set.add !used name;
        name)
    in
    levels.(level) <- special @ extra
  done;
  let shuffle lst =
    let arr = Array.of_list lst in
    for i = Array.length arr - 1 downto 1 do
      let j = rand_int (i + 1) in
      Array.swap arr i j
    done;
    Array.to_list arr
  in
  (* Generate edges ensuring all children are reachable *)
  let edges = Hashtbl.create (module String) in
  for level = 0 to depth - 2 do
    let parents = levels.(level) in
    let children = levels.(level + 1) in
    (* Ensure every child has at least one parent *)
    List.iter children ~f:(fun child ->
      let parent = List.nth_exn parents (rand_int (List.length parents)) in
      Hashtbl.update edges parent ~f:(function
        | None -> [ child ]
        | Some existing ->
          if List.mem existing child ~equal:String.equal
          then existing
          else child :: existing));
    (* Then add random additional edges up to max_fanout *)
    List.iter parents ~f:(fun parent ->
      let current = Hashtbl.find edges parent |> Option.value ~default:[] in
      let remaining =
        List.filter children ~f:(fun c -> not (List.mem current c ~equal:String.equal))
      in
      let max_additional = max 0 (max_fanout - List.length current) in
      let num_additional =
        if max_additional > 0 then rand_int (max_additional + 1) else 0
      in
      let additional = List.take (shuffle remaining) num_additional in
      Hashtbl.set edges ~key:parent ~data:(current @ additional))
  done;
  let buf = Buffer.create 256 in
  for level = 0 to depth - 2 do
    List.iter levels.(level) ~f:(fun node ->
      match Hashtbl.find edges node with
      | Some children when not (List.is_empty children) ->
        Buffer.add_string buf node;
        Buffer.add_char buf ':';
        List.iter children ~f:(fun c ->
          Buffer.add_char buf ' ';
          Buffer.add_string buf c);
        Buffer.add_char buf '\n'
      | _ -> ())
  done;
  Buffer.contents buf
;;

let%expect_test "create_random_graph generates valid graph" =
  let graph = create_random_graph ~seed:42 ~max_fanout:10 ~depth:5 in
  print_string graph;
  let lines = String.split_lines graph in
  let first_line = List.hd_exn lines in
  assert (String.is_prefix first_line ~prefix:"svr:");
  assert (List.exists lines ~f:(fun l -> String.is_prefix l ~prefix:"dac:"));
  assert (List.exists lines ~f:(fun l -> String.is_prefix l ~prefix:"fft:"));
  assert (String.is_substring graph ~substring:"out");
  let reference = Day_eleven.reference_solution graph in
  assert (reference.part_one <> 0);
  assert (reference.part_two <> 0);
  [%expect
    {|
    svr: quq rnk itp pjb kaj cxo fft
    fft: rad sjh eph dac doz gbc rxi
    cxo: doz dac sjh rxi rad
    kaj: sjh
    pjb: rxi eph doz gbc
    itp: gbc rad
    rnk: rxi sjh gbc dac rad doz eph
    quq: dac rad doz rxi gbc sjh
    dac: ehq shf box jur lrb irz bwn vko
    gbc: lrb vko bwn shf irz ehq jur
    eph: bwn irz box shf ehq lrb vko
    rxi: ehq shf irz box
    doz: irz vko jur shf box lrb bwn ehq
    sjh: lrb shf jur vko box ehq
    rad: jur vko box lrb ehq shf bwn
    jur: out
    box: out
    bwn: out
    shf: out
    lrb: out
    ehq: out
    vko: out
    irz: out
    |}]
;;

let%expect_test "small random graphs" =
  List.iter [ 42; 123; 999 ] ~f:(fun seed ->
    let graph = create_random_graph ~seed ~max_fanout:10 ~depth:5 in
    test_count_paths graph);
  [%expect
    {|
    ((part_one 213) (part_two 8))
    ((part_one 118) (part_two 4))
    ((part_one 51) (part_two 2))
    |}]
;;

let%expect_test "large random graphs" =
  List.iter [ 42; 123; 999 ] ~f:(fun seed ->
    let graph = create_random_graph ~seed ~max_fanout:16 ~depth:9 in
    test_count_paths graph);
  [%expect
    {|
    ((part_one 395600) (part_two 0))
    ((part_one 48057) (part_two 1488))
    ((part_one 34104) (part_two 408))
    |}]
;;
