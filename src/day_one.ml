open! Core
open! Hardcaml
open! Signal
module Divider = Hardcaml_circuits.Divider

(*
   * Advent of Code 2025 - Day 1: Secret Entrance
   *
   * A circular dial (0-99) starts at position 50. Given a sequence of rotations
   * (L/R followed by the number of ticks), simulate turning the dial left (toward lower
   * numbers) or right (toward higher numbers), wrapping around at boundaries.
   *
   * Part 1: Count how many times the dial lands exactly on 0 after any rotation.
   *
   * Part 2: Count every tick that ends on 0, including in the middle of a rotation
   * (not just at the end). A single large rotation can cross 0 multiple times.
*)

type solution =
  { part_one : int
  ; part_two : int
  }
[@@deriving sexp_of]

let reference_solution input =
  let rem_euclid a b = ((a mod b) + b) mod b in
  input
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
    let line = String.strip line in
    if String.is_empty line then None else Some line)
  |> List.fold ~init:(50, 0, 0) ~f:(fun (dial, z1, z2) line ->
    let steps = Int.of_string (String.drop_prefix line 1) in
    match line.[0] with
    | 'R' ->
      let new_dial = rem_euclid (dial + steps) 100 in
      let z1 = if new_dial = 0 then z1 + 1 else z1 in
      let z2 = z2 + ((steps + dial) / 100) in
      new_dial, z1, z2
    | 'L' ->
      let new_dial = rem_euclid (dial - steps) 100 in
      let z1 = if new_dial = 0 then z1 + 1 else z1 in
      let dist_from_hundred = rem_euclid (100 - dial) 100 in
      let z2 = z2 + ((steps + dist_from_hundred) / 100) in
      new_dial, z1, z2
    | c -> failwith (sprintf "Invalid direction: %c" c))
  |> fun (_dial, z1, z2) -> { part_one = z1; part_two = z2 }
;;

let value_width = 12 (* enough to contain dial position +-1000 signed *)
let width = value_width + 1 (* the direction bit *)

module Div = Divider.Make (struct
    let width = value_width
    let signedness = Signedness.Signed
    let architecture = Divider.Architecture.Combinational
  end)

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
      { part_one : 'a [@bits width]
      ; part_two : 'a [@bits width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include With_valid.Wrap.Make (Results)
end

module States = struct
  type t =
    | Read_dir
    | Read_ticks
    | End
  [@@deriving sexp_of, compare ~localize, enumerate, string, variants]
end

let create scope ({ clock; clear; byte_in } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let%hw.Always.State_machine sm =
    State_machine.create ~auto_wave_format:true (module States) spec
  in
  let%hw_var is_left_rotation = Variable.reg ~width:1 spec in
  let%hw_var ticks = Variable.reg ~width:value_width spec in
  let%hw_var instruction_valid = Variable.wire ~default:gnd () in
  let dial_init = Bits.of_unsigned_int ~width:value_width 50 in
  let%hw_var dial =
    Variable.reg ~width:value_width ~initialize_to:dial_init ~reset_to:dial_init spec
  in
  let%hw_var zeroes_part_one = Variable.reg ~width spec in
  let%hw_var zeroes_part_two = Variable.reg ~width spec in
  let%hw dial_turn = mux2 is_left_rotation.value (negate ticks.value) ticks.value in
  let hundred = of_int_trunc ~width:value_width 100 in
  let dial_divider =
    Div.create
      (Scope.sub_scope scope "dial_divider")
      { Div.I.clock
      ; clear
      ; numerator = dial.value +: dial_turn
      ; denominator = hundred
      ; start = vdd
      }
  in
  let remainder = dial_divider.remainder in
  let rem_w = Signal.width remainder in
  let positive_remainder =
    mux2 (Signal.msb remainder) (remainder +: of_int_trunc ~width:rem_w 100) remainder
  in
  let%hw new_dial = sel_bottom positive_remainder ~width:value_width in
  (* PART TWO signals *)
  let distance_from_hundred =
    mux2 (dial.value ==:. 0) (zero value_width) (hundred -: dial.value)
  in
  let%hw start = mux2 is_left_rotation.value distance_from_hundred dial.value in
  let part_two_divider =
    Div.create
      (Scope.sub_scope scope "part_two_divider")
      { Div.I.clock
      ; clear
      ; numerator = start +: ticks.value
      ; denominator = hundred
      ; start = vdd
      }
  in
  let%hw quotient = part_two_divider.quotient in
  let%hw euclidean_quotient = mux2 (msb quotient) (negate quotient +:. 1) quotient in
  compile
    [ sm.switch
        [ ( Read_dir
          , [ when_
                byte_in.valid
                [ if_
                    (byte_in.value ==: of_char 'L')
                    [ is_left_rotation <--. 1; ticks <--. 0; sm.set_next Read_ticks ]
                  @@ elif
                       (byte_in.value ==: of_char 'R')
                       [ is_left_rotation <--. 0; ticks <--. 0; sm.set_next Read_ticks ]
                  @@ elif (byte_in.value ==: of_char '\n') [ sm.set_next End ] []
                ]
            ] )
        ; ( Read_ticks
          , [ when_
                byte_in.valid
                [ if_
                    (byte_in.value ==: of_char '\n')
                    [ instruction_valid <-- vdd
                    ; sm.set_next Read_dir
                    ; dial <-- new_dial
                    ; when_
                        (new_dial ==:. 0)
                        [ zeroes_part_one <-- zeroes_part_one.value +:. 1 ]
                    ; zeroes_part_two <-- zeroes_part_two.value +: euclidean_quotient
                    ]
                    [ ticks
                      <-- sel_bottom
                            (ticks.value *: of_int_trunc ~width:value_width 10)
                            ~width:value_width
                          +: uresize ~width:value_width (byte_in.value -: of_char '0')
                    ]
                ]
            ] )
        ; End, []
        ]
    ];
  let _ = instruction_valid in
  { With_valid.valid = sm.is End
  ; value =
      { O.Results.part_one = zeroes_part_one.value; part_two = zeroes_part_two.value }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day_one" create
;;
