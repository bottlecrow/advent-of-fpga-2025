open! Core
open! Hardcaml
open! Signal

(*
   * Advent of Code 2025 - Day 7: Laboratories
   *
   * A tachyon beam enters a 2D grid at position S and travels downward. Empty space
   * (.) is traversed freely. Splitters (^) stop the beam and emit two new beams
   * continuing downward from the immediate left and right of the splitter. Beams
   * that exit the grid or hit a splitter terminate.
   *
   * Part 1: Count the total number of times a beam is split (i.e., how many
   * times a splitter is hit by a beam).
   *
   * Part 2: At each splitter, the beam splits and follows both possible paths.
   * Count the total number of distinct paths the beam can take to completion.
*)

type solution =
  { part_one : int
  ; part_two : int
  }
[@@deriving sexp_of]

(** Software reference solution **)

type tile =
  | Empty
  | Splitter
  | Beam of int

let parse_input input =
  input
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
    let line = String.strip line in
    if String.is_empty line then None else Some line)
  |> Array.of_list
  |> Array.map ~f:(fun line ->
    String.to_array line
    |> Array.map ~f:(function
      | '.' -> Empty
      | '^' -> Splitter
      | 'S' -> Beam 1
      | c -> failwith (sprintf "Invalid tile: '%c'" c)))
;;

let propagate_beam tile strength =
  match tile with
  | Beam s -> Beam (s + strength)
  | Empty -> Beam strength
  | Splitter -> failwith "Cannot propagate beam into splitter"
;;

let simulate_row prev_row row =
  let n = Array.length row in
  let splits = ref 0 in
  Array.iteri prev_row ~f:(fun i prev_tile ->
    match prev_tile with
    | Beam strength ->
      if phys_equal row.(i) Splitter
      then (
        if i >= 1 then row.(i - 1) <- propagate_beam row.(i - 1) strength;
        if i + 1 < n then row.(i + 1) <- propagate_beam row.(i + 1) strength;
        Int.incr splits)
      else row.(i) <- propagate_beam row.(i) strength
    | _ -> ());
  !splits
;;

let reference_solution input =
  let grid = parse_input input in
  let splits = ref 0 in
  for i = 1 to Array.length grid - 1 do
    splits := !splits + simulate_row grid.(i - 1) grid.(i)
  done;
  let part_two =
    Array.last_exn grid
    |> Array.sum (module Int) ~f:(function
      | Beam s -> s
      | _ -> 0)
  in
  { part_one = !splits; part_two }
;;

(** Hardware implementation **)

module Uart_byte = With_valid.Vector (struct
    let width = 8
  end)

let width = 64
let grid_width = 160

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
      { part_one : 'a [@bits width]
      ; part_two : 'a [@bits width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include With_valid.Wrap.Make (Results)
end

module States = struct
  type t =
    | Beam_start
    | Splitting
    | End
  [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
end

let create scope ({ clock; clear; byte_in } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let%hw.Always.State_machine sm =
    State_machine.create ~auto_wave_format:true (module States) spec
  in
  let%hw_var col = Variable.reg ~width:(num_bits_to_represent grid_width) spec in
  let%hw_var_list beam_row =
    List.init grid_width ~f:(fun _ -> Variable.reg ~width spec)
  in
  let%hw_var_list splitter_row =
    List.init grid_width ~f:(fun _ -> Variable.reg ~width:1 spec)
  in
  let%hw_var_list split_count_row =
    List.init grid_width ~f:(fun _ -> Variable.reg ~width spec)
  in
  let update_beam_row () =
    let n = List.length beam_row in
    List.mapi beam_row ~f:(fun i beam ->
      let splitter_i = List.nth_exn splitter_row i in
      let counter_var = List.nth_exn split_count_row i in
      let left_contrib =
        if i = 0
        then Signal.zero width
        else (
          let splitter_left = (List.nth_exn splitter_row (i - 1)).value in
          let beam_left = (List.nth_exn beam_row (i - 1)).value in
          mux2 splitter_left beam_left (Signal.zero width))
      in
      let right_contrib =
        if i = n - 1
        then Signal.zero width
        else (
          let splitter_right = (List.nth_exn splitter_row (i + 1)).value in
          let beam_right = (List.nth_exn beam_row (i + 1)).value in
          mux2 splitter_right beam_right (Signal.zero width))
      in
      let new_beam_value = beam.value +: left_contrib +: right_contrib in
      [ beam <-- mux2 splitter_i.value (Signal.zero width) new_beam_value
        (* only count right splitter to avoid double counting *)
      ; when_ (right_contrib <>:. 0) [ counter_var <-- counter_var.value +:. 1 ]
      ])
    |> List.concat
  in
  let%hw total_splits =
    List.map split_count_row ~f:Variable.value
    |> Signal.tree ~arity:4 ~f:(Signal.reduce ~f:( +: ))
  in
  let%hw total_beams =
    List.map beam_row ~f:Variable.value
    |> Signal.tree ~arity:4 ~f:(Signal.reduce ~f:( +: ))
  in
  compile
    [ sm.switch
        [ ( Beam_start
          , [ when_
                byte_in.valid
                [ col <-- col.value +:. 1
                ; when_
                    (byte_in.value ==: Signal.of_char 'S')
                    (List.mapi beam_row ~f:(fun i var ->
                       when_ (col.value ==:. i) [ var <--. 1 ]))
                ; when_
                    (byte_in.value ==: Signal.of_char '\n')
                    [ sm.set_next Splitting; col <--. 0 ]
                ]
            ] )
        ; ( Splitting
          , [ when_
                byte_in.valid
                [ col <-- col.value +:. 1
                ; when_
                    (byte_in.value ==: Signal.of_char '^')
                    (List.mapi splitter_row ~f:(fun i var ->
                       when_ (col.value ==:. i) [ var <--. 1 ]))
                ; when_
                    (byte_in.value ==: Signal.of_char '\n')
                    ([ col <--. 0 ]
                     @ List.map splitter_row ~f:(fun var -> var <--. 0)
                     @ update_beam_row ())
                ; when_ (byte_in.value ==: Signal.of_char 'X') [ sm.set_next End ]
                ]
            ] )
        ; End, []
        ]
    ];
  { With_valid.valid = sm.is End
  ; value = { O.Results.part_one = total_splits; part_two = total_beams }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day_seven" create
;;
