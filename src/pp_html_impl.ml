module P = Reparse.Parser
open Sexplib.Std
open P.Infix

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

and attributes = (string * string option) list [@@deriving sexp_of]

let sp = Printf.sprintf
let skip_ws at_least = P.(skip ~at_least (any [ htab; lf; char '\x0C'; cr; space ]))

let comments =
  let open P in
  take_between ~start:(string "<!--") ~end_:(string "-->") next
  >>= string_of_chars
  >|= fun s -> Comments s
;;

let doctype =
  let open P in
  take_between
    next
    ~start:(string ~case_sensitive:false "<!DOCTYPE" *> skip_ws 1)
    ~end_:(char '>')
  *> unit
;;

let text =
  let open P in
  let* txt = take_while next ~while_:(is_not @@ char '<') >>= string_of_chars in
  if String.length txt > 0 then pure @@ Text txt else fail "Invalid HTML text node"
;;

let node =
  let open P in
  (* Void elements - https://html.spec.whatwg.org/multipage/syntax.html#void-elements *)
  let is_void = function
    | "area"
    | "base"
    | "br"
    | "col"
    | "embed"
    | "hr"
    | "img"
    | "input"
    | "link"
    | "meta"
    | "param"
    | "source"
    | "track"
    | "wbr" -> true
    | _ -> false
  in
  recur (fun node ->
      (* Parse and discard doctype node *)
      let* () = (optional @@ doctype) *> unit in
      (* 
         tag_name - https://html.spec.whatwg.org/multipage/syntax.html#syntax-tag-name
         start_tags - https://html.spec.whatwg.org/multipage/syntax.html#start-tags
      *)
      let* tag_name =
        skip_ws 0 *> char '<' *> take alpha_num
        >>= string_of_chars
        <?> sp "Invalid HTML tag"
      in
      (* let* attributes = attributes in *)
      let void = is_void tag_name in
      let* () =
        (if void then (optional @@ char '/') *> unit else unit) <* char '>' <* skip_ws 0
      in
      let closing_tag =
        string "</" *> string tag_name *> skip_ws 0 *> char '>'
        <?> sp "Closing tag '</%s>' missing." tag_name
      in
      (if void
      then pure @@ Void { tag_name; attributes = [] }
      else
        let+ children =
          take @@ (any [ comments; node; text ] <* skip_ws 0) <* closing_tag
        in
        Element { tag_name; attributes = []; children })
      <* skip_ws 0)
;;

let parse s = P.parse_string node s

module F = Format

(* module E = Easy_format *)

let pp fmt = function
  | Text txt -> F.fprintf fmt "%s" txt
  | Element { tag_name; _ } -> F.fprintf fmt "<%s>" tag_name
  | _ -> ()
;;
