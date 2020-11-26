module P = Reparse.Parser
open Sexplib.Std

type node =
  | Text of string
  | Comments of string
  | Void of
      { tag_name : string
      ; attributes : attributes
      }
  | Element of
      { tag_name : string
      ; attributes : attributes
      ; children : node list
      }

and attributes = (string * string) list [@@deriving sexp_of]

let parse s = Text s

module F = Format

let pp fmt = function
  | Text txt -> F.fprintf fmt "%s" txt
  | Element { tag_name; _ } -> F.fprintf fmt "<%s>" tag_name
  | _ -> ()
;;

let%expect_test "element" =
  parse "<html></html>" |> pp Format.std_formatter;
  [%expect {||}]
;;
