open! Core
open! Hardcaml
open! Signal

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
  let part_one = count "you" "out" in
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
  { read_addr; read_data = memory.qa; write_addr; write_data; write_enable }
;;

module Init_graph = struct
  module States = struct
    type t =
      | Parent_node
      | Child_node
    [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
  end

  type 'a vars =
    { sm : 'a Always.State_machine.t
    ; id_counter : Always.Variable.t
    ; parent_node_id : Always.Variable.t
    ; parent_name_id : Always.Variable.t
    ; child_name_id : Always.Variable.t
    ; child_node_id : Always.Variable.t
    ; edge_counter : Always.Variable.t
    ; node_counter : Always.Variable.t
    ; i : Always.Variable.t
    }

  let create_vars scope spec =
    let open Always in
    let scope = Scope.sub_scope scope "init_graph_vars" in
    let%hw.Always.State_machine sm =
      State_machine.create ~auto_wave_format:true (module States) spec
    in
    let%hw_var id_counter = Variable.reg ~width:node_id_width spec in
    let%hw_var parent_name_id = Variable.reg ~width:name_id_width spec in
    let%hw_var parent_node_id = Variable.reg ~width:node_id_width spec in
    let%hw_var child_name_id = Variable.reg ~width:name_id_width spec in
    let%hw_var child_node_id = Variable.reg ~width:node_id_width spec in
    let%hw_var edge_counter = Variable.reg ~width:edge_index_width spec in
    let%hw_var node_counter = Variable.reg ~width:node_id_width spec in
    let%hw_var i = Variable.reg ~width:3 spec in
    { sm
    ; id_counter
    ; parent_name_id
    ; parent_node_id
    ; child_name_id
    ; child_node_id
    ; edge_counter
    ; node_counter
    ; i
    }
  ;;

  let handle
    (byte : _ Uart_byte.t)
    names
    nodes
    edges
    ({ sm
     ; id_counter
     ; parent_name_id
     ; parent_node_id
     ; child_name_id
     ; child_node_id
     ; edge_counter
     ; node_counter
     ; i
     } :
      States.t vars)
    exit_state_machine
    : Always.t list
    =
    let char_to_ord c = uresize ~width:name_id_width (c -: of_char 'a') in
    let open Always in
    (* 0 is not a valid id, so we add 1 to the counter _before_ assigning it to a name *)
    let next_id = id_counter.value +:. 1 in
    let node_edges_start_offset =
      (parent_node_id.value -:. 1) *: of_int_trunc ~width:node_id_width 2
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
    | Topo_sort
    | Count_paths
    | End
  [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
end

let handle_topo_sort (sm : States.t Always.State_machine.t) : Always.t list =
  [ sm.set_next Count_paths ]
;;

let handle_count_paths (sm : States.t Always.State_machine.t) : Always.t list =
  [ sm.set_next End ]
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
    let address_bits = node_id_width * 2
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
  let init_graph_vars = Init_graph.create_vars scope spec in
  let%hw_var done_signal = Variable.wire ~default:Signal.gnd () in
  let%hw_var part_one = Variable.wire ~default:(Signal.zero output_width) () in
  let%hw_var part_two = Variable.wire ~default:(Signal.zero output_width) () in
  let%hw.Always.State_machine sm =
    State_machine.create ~auto_wave_format:true (module States) spec
  in
  compile
    [ sm.switch
        [ ( Init_graph
          , [ proc
              @@ Init_graph.handle
                   byte_in
                   names
                   nodes
                   edges
                   init_graph_vars
                   (sm.set_next Topo_sort)
            ] )
        ; Topo_sort, handle_topo_sort sm
        ; Count_paths, handle_count_paths sm
        ; ( End
          , [ nodes.read_addr <--. 1
            ; edges.read_addr <--. 1
            ; part_one <-- uresize ~width:output_width nodes.read_data
            ; part_two <-- uresize ~width:output_width edges.read_data
            ; done_signal <-- vdd
            ] )
        ]
    ];
  { With_valid.valid = done_signal.value
  ; value =
      { O.Results.part_one = part_one.value
      ; part_two = part_two.value
      ; state = sm.current
      }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day_eleven" create
;;
