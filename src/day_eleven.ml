open! Core
open! Hardcaml
open! Signal

(*
   * Advent of Code 2025 - Day 11: Reactor
   *
   * A directed acyclic graph represents devices connected by outputs. Each line
   * of input defines a device and its connections: "name: dest1 dest2 dest3".
   *
   * Part 1: Count the total number of distinct paths from "svr" to "out".
   *
   * Part 2: Count the number of distinct paths from "svr" to "out" that pass
   * through both the "dac" and "fft" devices (in any order).
*)

type solution =
  { part_one : int
  ; part_two : int
  }
[@@deriving sexp_of]

let parse_input input =
  String.split_lines input
  |> List.filter_map ~f:(fun line ->
    match String.lsplit2 line ~on:':' with
    | Some (src, dsts) ->
      let dests = String.split dsts ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
      Some (String.strip src, dests)
    | None -> None)
  |> Map.of_alist_exn (module String)
;;

let topo_sort graph =
  let in_deg = Hashtbl.create (module String) in
  Map.iter
    graph
    ~f:
      (List.iter ~f:(fun d ->
         Hashtbl.update in_deg d ~f:(fun v -> 1 + Option.value v ~default:0)));
  let queue = Queue.create () in
  Map.iter_keys graph ~f:(fun n ->
    if not (Hashtbl.mem in_deg n) then Queue.enqueue queue n);
  let rec drain acc =
    match Queue.dequeue queue with
    | None -> List.rev acc
    | Some cur ->
      Map.find graph cur
      |> Option.iter
           ~f:
             (List.iter ~f:(fun d ->
                Hashtbl.decr in_deg d;
                if Hashtbl.find_exn in_deg d = 0 then Queue.enqueue queue d));
      drain (cur :: acc)
  in
  drain []
;;

let count_paths_topo ~graph ~topo_order ~topo_map start finish =
  match Map.find topo_map start, Map.find topo_map finish with
  | Some start_idx, Some finish_idx when start_idx <= finish_idx ->
    let counts =
      List.fold_right
        topo_order
        ~init:(Map.empty (module String))
        ~f:(fun node counts ->
          let node_idx = Map.find_exn topo_map node in
          if node_idx > finish_idx || node_idx < start_idx
          then counts
          else if String.equal node finish
          then Map.set counts ~key:node ~data:1
          else (
            let total =
              Map.find graph node
              |> Option.value ~default:[]
              |> List.sum (module Int) ~f:(fun d ->
                Map.find counts d |> Option.value ~default:0)
            in
            Map.set counts ~key:node ~data:total))
    in
    Map.find_exn counts start
  | _ -> 0
;;

let reference_solution input =
  let graph = parse_input input in
  let topo_order = topo_sort graph in
  let topo_map =
    List.mapi topo_order ~f:(fun i n -> n, i) |> Map.of_alist_exn (module String)
  in
  let count = count_paths_topo ~graph ~topo_order ~topo_map in
  let part_one = count "svr" "out" in
  let part_two =
    match Map.find topo_map "fft", Map.find topo_map "dac" with
    | Some fft_idx, Some dac_idx ->
      if fft_idx < dac_idx
      then count "svr" "fft" * count "fft" "dac" * count "dac" "out"
      else count "svr" "dac" * count "dac" "fft" * count "fft" "out"
    | _ -> 0
  in
  { part_one; part_two }
;;

(** Hardware implementation **)

let output_width = 32
let max_nodes = 1024
let node_id_width = Signal.num_bits_to_represent (max_nodes - 1)
let max_node_array_len = max_nodes * 2
let nodes_array_width = Signal.num_bits_to_represent (max_node_array_len - 1)
let max_edges = max_nodes * 16
let edge_index_width = Signal.num_bits_to_represent (max_edges - 1)
let max_children_per_node = 32
let max_name_id = Int.pow 26 3
let name_id_width = Signal.num_bits_to_represent (max_name_id - 1)

module Uart_byte = With_valid.Vector (struct
    let width = 8
  end)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; byte_in : 'a Uart_byte.t
    }
  [@@deriving hardcaml]
end

module O = struct
  module Results = struct
    type 'a t =
      { part_one : 'a [@bits output_width]
      ; part_two : 'a [@bits output_width]
      ; state : 'a [@bits 2] (* States.t has 4 variants, so 2 bits *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  include With_valid.Wrap.Make (Results)
end

type 'a memory =
  { read_addr : Always.Variable.t
  ; read_data : 'a
  ; write_addr : Always.Variable.t
  ; write_data : Always.Variable.t
  ; write_enable : Always.Variable.t
  }

let create_memory ~scope ~name ~clock (module Config : Ram.Dual_port.Config) =
  let scope = Scope.sub_scope scope name in
  let module Memory = Ram.Dual_port.Make (Config) in
  let open Always in
  let zero_addr_wire () = Variable.wire ~default:(Signal.zero Config.address_bits) () in
  let zero_data_wire () = Variable.wire ~default:(Signal.zero Config.data_bits) () in
  let%hw_var read_addr = zero_addr_wire () in
  let port_a =
    { Memory.Port.address = read_addr.value
    ; data = Signal.zero Config.data_bits
    ; enable = Signal.vdd
    ; write = Signal.gnd
    }
  in
  let%hw_var write_addr = zero_addr_wire () in
  let%hw_var write_data = zero_data_wire () in
  let%hw_var write_enable = Variable.wire ~default:Signal.gnd () in
  let port_b =
    { Memory.Port.address = write_addr.value
    ; data = write_data.value
    ; enable = write_enable.value
    ; write = Signal.vdd
    }
  in
  let memory =
    Memory.hierarchical
      ~name
      ~memory_name:name
      (Scope.sub_scope scope name)
      { Memory.I.clock; port_a; port_b }
  in
  let%hw read_data = memory.qa in
  { read_addr; read_data; write_addr; write_data; write_enable }
;;

module Init_graph = struct
  module States = struct
    type t =
      | Parent_node
      | Child_node
    [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
  end

  let handle
    ~scope
    ~spec
    ~(sm : States.t Always.State_machine.t)
    (byte : _ Uart_byte.t)
    names
    nodes
    edges
    exit_state_machine
    : Always.t list
    =
    let scope = Scope.sub_scope scope "init_graph" in
    let open Always in
    let%hw_var id_counter = Variable.reg ~width:node_id_width spec in
    let%hw_var parent_name_id = Variable.reg ~width:name_id_width spec in
    let%hw_var parent_node_id = Variable.reg ~width:node_id_width spec in
    let%hw_var child_name_id = Variable.reg ~width:name_id_width spec in
    let%hw_var child_node_id = Variable.reg ~width:node_id_width spec in
    let%hw_var edge_counter = Variable.reg ~width:edge_index_width spec in
    let%hw_var node_counter = Variable.reg ~width:node_id_width spec in
    let%hw_var i = Variable.reg ~width:3 spec in
    let char_to_ord c = uresize ~width:name_id_width (c -: of_char 'a') in
    let open Always in
    (* 0 is not a valid id, so we add 1 to the counter _before_ assigning it to a name *)
    let next_id = id_counter.value +:. 1 in
    let node_edges_start_offset =
      uresize
        ~width:nodes_array_width
        ((parent_node_id.value -:. 1) *: of_int_trunc ~width:nodes_array_width 2)
    in
    let node_edges_end_offset = node_edges_start_offset +:. 1 in
    let write_name addr id =
      proc
        [ names.write_addr <-- addr; names.write_data <-- id; names.write_enable <-- vdd ]
    in
    let map_name_to_node name_id_value node_id cb =
      proc
        [ when_ (i.value ==:. 3) [ i <--. 4 ] (* delay one cycle to read from names *)
        ; when_
            (i.value ==:. 4)
            [ if_
                (names.read_data ==:. 0)
                [ write_name name_id_value next_id
                ; node_id <-- next_id
                ; id_counter <-- next_id
                ]
              @@ else_ [ node_id <-- names.read_data ]
            ; i <--. 5
            ]
        ; when_ (i.value ==:. 5) (cb @ [ i <--. 0 ])
        ]
    in
    [ sm.switch
        [ ( Parent_node
          , [ when_
                byte.valid
                [ if_ (byte.value ==: of_char ':') [ sm.set_next Child_node ]
                  @@ elif (byte.value ==: of_char '\n') [ exit_state_machine ]
                  @@ else_
                       [ parent_name_id
                         <-- uresize
                               ~width:name_id_width
                               (parent_name_id.value
                                *: of_int_trunc ~width:name_id_width 26)
                             +: char_to_ord byte.value
                       ; i <-- i.value +:. 1
                       ]
                ]
            ; names.read_addr <-- parent_name_id.value
            ; map_name_to_node
                parent_name_id.value
                parent_node_id
                [ nodes.write_addr <-- node_edges_start_offset
                ; nodes.write_enable <-- vdd
                ; nodes.write_data <-- edge_counter.value
                ]
            ] )
        ; ( Child_node
          , [ when_
                byte.valid
                [ if_
                    (byte.value ==: of_char '\n')
                    [ sm.set_next Parent_node
                    ; nodes.write_addr <-- node_edges_end_offset
                    ; nodes.write_enable <-- vdd
                    ; nodes.write_data <-- edge_counter.value -: nodes.read_data
                    ; parent_name_id <--. 0
                    ; node_counter <-- node_counter.value +:. 1
                    ]
                  @@ elif
                       (byte.value ==: of_char ' ')
                       [ child_name_id <--. 0; child_node_id <--. 0; i <--. 0 ]
                  @@ else_
                       [ child_name_id
                         <-- uresize
                               ~width:name_id_width
                               (child_name_id.value
                                *: of_int_trunc ~width:name_id_width 26)
                             +: char_to_ord byte.value
                       ; i <-- i.value +:. 1
                       ]
                ]
            ; names.read_addr <-- child_name_id.value
            ; map_name_to_node
                child_name_id.value
                child_node_id
                [ edges.write_addr <-- edge_counter.value
                ; edges.write_enable <-- vdd
                ; edges.write_data <-- child_node_id.value
                ; edge_counter <-- edge_counter.value +:. 1
                ]
            ] )
        ]
    ]
  ;;
end

module States = struct
  type t =
    | Init_graph
    | Setup_search
    | Count_paths
    | End
  [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
end

module Dfs_stack = struct
  module Entry = struct
    type 'a t =
      { node_id : 'a [@bits node_id_width]
      ; seen : 'a [@bits 2]
      }
    [@@deriving hardcaml]
  end

  module Entry_with_valid = With_valid.Wrap.Make (Entry)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a Entry_with_valid.t
      ; pop : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { top : 'a Entry_with_valid.t
      ; size : 'a [@bits node_id_width]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) : _ O.t =
    let scope = Scope.sub_scope scope "dfs_stack" in
    let spec = Reg_spec.create ~clock:i.clock () in
    let%hw top_idx = wire node_id_width in
    let is_empty = top_idx ==:. 0 in
    let%hw top_next = top_idx +:. 1 in
    let%hw top_prev = top_idx -:. 1 in
    let%hw push = i.push.valid in
    let%hw pop = i.pop &: ~:is_empty in
    top_idx
    <-- reg spec ~enable:(push ^: pop) ~clear:i.clear (mux2 push top_next top_prev);
    let create_stack_slice write_data =
      (multiport_memory
         ~name:"stack_mem"
         max_nodes
         ~write_ports:
           [| { write_clock = i.clock
              ; write_enable = push
              ; write_address = top_next
              ; write_data
              }
           |]
         ~read_addresses:[| top_idx |]).(0)
    in
    let%hw.Entry.Of_signal top = Entry.map i.push.value ~f:create_stack_slice in
    { O.top = { With_valid.valid = ~:is_empty; value = top }; size = top_idx }
  ;;
end

(* Sequence always actions based on a counter *)
let sequence ~spec (steps : Always.t list list) =
  let open Always in
  let n = List.length steps in
  let i = Variable.reg ~width:(num_bits_to_represent n) spec in
  List.mapi steps ~f:(fun j step ->
    let inc_i = if j = n - 1 then i <--. 0 else i <--. j + 1 in
    when_ (i.value ==:. j) (step @ [ inc_i ]))
;;

module Count_paths = struct
  module States = struct
    type t =
      | Pop_node
      | Add_edges
      | End
    [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
  end

  type vars =
    { push : Always.Variable.t Dfs_stack.Entry_with_valid.t
    ; path_count : Always.Variable.t
    ; path_count_with_mid_nodes : Always.Variable.t
    ; key_nodes : Always.Variable.t array
    }

  let create_vars ~clock scope =
    let open Always in
    let scope = Scope.sub_scope scope "count_paths" in
    let spec = Reg_spec.create ~clock () in
    let push = Dfs_stack.Entry_with_valid.Of_always.wire Signal.zero in
    let%hw_var path_count = Variable.reg ~width:output_width spec in
    let%hw_var path_count_with_mid_nodes = Variable.reg ~width:output_width spec in
    let%hw_var_array key_nodes =
      Array.init 3 ~f:(fun _ -> Variable.reg ~width:node_id_width spec)
    in
    { push; path_count; path_count_with_mid_nodes; key_nodes }
  ;;

  let handle
    ~clock
    ~scope
    ~(sm : States.t Always.State_machine.t)
    (v : vars)
    nodes
    edges
    exit_state
    : Always.t list
    =
    let scope = Scope.sub_scope scope "count_paths_handle" in
    let spec = Reg_spec.create ~clock () in
    let open Always in
    let%hw_var pop = Variable.wire ~default:Signal.gnd () in
    let stack =
      Dfs_stack.create
        scope
        { clock
        ; clear = Signal.gnd
        ; push = Dfs_stack.Entry_with_valid.Of_always.value v.push
        ; pop = pop.value
        }
    in
    let top = stack.top in
    let%hw.Dfs_stack.Entry.Of_always search_node = Dfs_stack.Entry.Of_always.reg spec in
    let%hw_var cur_edge = Variable.reg ~width:edge_index_width spec in
    let%hw is_mid_node =
      search_node.node_id.value
      ==: v.key_nodes.(0).value
      |: (search_node.node_id.value ==: v.key_nodes.(1).value)
    in
    let%hw is_end_node = search_node.node_id.value ==: v.key_nodes.(2).value in
    let saw_mid_nodes = search_node.seen.value ==:. 2 in
    let edges_start_offset =
      uresize
        ~width:nodes_array_width
        ((search_node.node_id.value -:. 1) *: of_int_trunc ~width:nodes_array_width 2)
    in
    let edges_end_offset = edges_start_offset +:. 1 in
    let edges_end = Variable.reg ~width:edge_index_width spec in
    [ sm.switch
        [ ( Pop_node
          , sequence
              ~spec
              [ [ if_ ~:(top.valid) [ exit_state ]
                  @@ else_
                       [ pop <-- vdd
                       ; Dfs_stack.Entry.Of_always.assign search_node top.value
                       ]
                ]
              ; [ nodes.read_addr <-- edges_start_offset
                ; when_
                    is_end_node
                    [ v.path_count <-- v.path_count.value +:. 1
                    ; when_
                        saw_mid_nodes
                        [ v.path_count_with_mid_nodes
                          <-- v.path_count_with_mid_nodes.value +:. 1
                        ]
                    ]
                ; when_ is_mid_node [ search_node.seen <-- search_node.seen.value +:. 1 ]
                ]
              ; [ cur_edge <-- nodes.read_data; nodes.read_addr <-- edges_end_offset ]
              ; [ edges_end <-- nodes.read_data; sm.set_next Add_edges ]
              ] )
        ; ( Add_edges
          , [ if_ (cur_edge.value ==: edges_end.value) [ sm.set_next Pop_node ]
              @@ else_
              @@ sequence
                   ~spec
                   [ [ edges.read_addr <-- cur_edge.value ]
                   ; [ v.push.value.node_id <-- edges.read_data
                     ; v.push.value.seen <-- search_node.seen.value
                     ; v.push.valid <-- vdd
                     ; cur_edge <-- cur_edge.value +:. 1
                     ]
                   ]
            ] )
        ; End, [ exit_state ]
        ]
    ]
  ;;
end

let name_index name =
  String.to_sequence name
  |> Sequence.fold ~init:0 ~f:(fun acc c ->
    (acc * 26) + (Char.to_int c - Char.to_int 'a'))
;;

let create scope ({ clock; clear; byte_in } : _ I.t) : _ O.t =
  let spec = Signal.Reg_spec.create ~clock ~clear () in
  let open Always in
  let module Names_config = struct
    let address_bits = name_id_width
    let data_bits = node_id_width
  end
  in
  let module Nodes_config = struct
    let address_bits = nodes_array_width
    let data_bits = edge_index_width
  end
  in
  let module Edges_config = struct
    let address_bits = edge_index_width
    let data_bits = node_id_width
  end
  in
  let names = create_memory ~scope ~name:"names" ~clock (module Names_config) in
  let nodes = create_memory ~scope ~name:"nodes" ~clock (module Nodes_config) in
  let edges = create_memory ~scope ~name:"edges" ~clock (module Edges_config) in
  let count_paths_vars = Count_paths.create_vars ~clock scope in
  let%hw_var done_signal = Variable.wire ~default:Signal.gnd () in
  let%hw.Always.State_machine sm =
    State_machine.create ~auto_wave_format:true (module States) spec
  in
  let%hw.Always.State_machine init_graph_sm =
    State_machine.create ~auto_wave_format:true (module Init_graph.States) spec
  in
  let%hw.Always.State_machine count_paths_sm =
    State_machine.create ~auto_wave_format:true (module Count_paths.States) spec
  in
  let push = count_paths_vars.push in
  let key_nodes = count_paths_vars.key_nodes in
  compile
    [ sm.switch
        [ ( Init_graph
          , Init_graph.handle
              ~scope
              ~spec
              ~sm:init_graph_sm
              byte_in
              names
              nodes
              edges
              (sm.set_next Setup_search) )
        ; ( Setup_search
          , sequence
              ~spec
              [ [ names.read_addr <-- of_int_trunc ~width:name_id_width (name_index "svr")
                ]
              ; [ push.value.node_id <-- names.read_data
                ; push.value.seen <-- of_int_trunc ~width:2 0
                ; push.valid <-- vdd
                ; names.read_addr <-- of_int_trunc ~width:name_id_width (name_index "dac")
                ]
              ; [ key_nodes.(0) <-- names.read_data
                ; names.read_addr <-- of_int_trunc ~width:name_id_width (name_index "fft")
                ]
              ; [ key_nodes.(1) <-- names.read_data
                ; names.read_addr <-- of_int_trunc ~width:name_id_width (name_index "out")
                ]
              ; [ key_nodes.(2) <-- names.read_data; sm.set_next Count_paths ]
              ] )
        ; ( Count_paths
          , Count_paths.handle
              ~clock
              ~scope
              ~sm:count_paths_sm
              count_paths_vars
              nodes
              edges
              (sm.set_next End) )
        ; End, [ done_signal <-- vdd ]
        ]
    ];
  { With_valid.valid = done_signal.value
  ; value =
      { O.Results.part_one = count_paths_vars.path_count.value
      ; part_two = count_paths_vars.path_count_with_mid_nodes.value
      ; state = sm.current
      }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day_eleven" create
;;
