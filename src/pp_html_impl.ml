module P = Reparse.Parser
open Sexplib.Std
open P
open P.Infix

type node =
  | Text of string
  | Comments of string
  | Void of
      { tag_name : string
      ; attributes : attribute list
      }
  | Element of
      { tag_name : string
      ; attributes : attribute list
      ; children : node list
      }

and attribute = Attr of (string * string option) [@@unboxed] [@@deriving sexp_of]

let sp = Printf.sprintf
let skip_ws at_least = P.(skip ~at_least (any [ htab; lf; char '\x0C'; cr; space ]))

let comments =
  take_between ~start:(string "<!--") ~end_:(string "-->") next
  >>= string_of_chars
  >|= fun s -> Comments s
;;

let doctype =
  take_between
    next
    ~start:(string ~case_sensitive:false "<!DOCTYPE" *> skip_ws 1)
    ~end_:(char '>')
  >>= string_of_chars
;;

let text =
  let* txt = take_while next ~while_:(is_not @@ char '<') >>= string_of_chars in
  if String.length txt > 0 then pure @@ Text txt else fail "Invalid HTML text node"
;;

let attributes =
  let attribute =
    let illegal_attr_name_char =
      char_if (function
          | '\x00' .. '\x1F' | '\x7F' .. '\x9F' | ' ' | '"' | '\'' | '>' | '/' | '=' ->
            true
          | _ -> false)
    in
    let* attr_name =
      take_while next ~while_:(is_not illegal_attr_name_char) >>= string_of_chars
    in
    let+ attr_val =
      (let* () = skip_ws 0 *> char '=' *> skip_ws 0 *> unit in
       let* delimiter = optional @@ any [ char '"'; char '\'' ] in
       match delimiter with
       | Some c ->
         take_while next ~while_:(is_not @@ char c) <* char c >>= string_of_chars
       | None -> take_while next ~while_:(is_not space) >>= string_of_chars)
      |> optional
    in
    Attr (attr_name, attr_val)
  in
  optional @@ skip_ws 1
  >>= function
  | Some _ -> take attribute ~sep_by:space
  | None -> pure []
;;

let node =
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
      (* 
         tag_name - https://html.spec.whatwg.org/multipage/syntax.html#syntax-tag-name
         start_tags - https://html.spec.whatwg.org/multipage/syntax.html#start-tags
      *)
      let* tag_name =
        skip_ws 0 *> char '<' *> take alpha_num
        >>= string_of_chars
        <?> sp "Invalid HTML tag"
      in
      let* attributes = attributes in
      let void = is_void tag_name in
      let* () =
        (if void then (optional @@ char '/') *> unit else unit) <* char '>' <* skip_ws 0
      in
      let closing_tag =
        string "</" *> string tag_name *> skip_ws 0 *> char '>'
        <?> sp "Closing tag '</%s>' missing." tag_name
      in
      (if void
      then pure @@ Void { tag_name; attributes }
      else
        let+ children =
          take @@ (any [ comments; node; text ] <* skip_ws 0) <* closing_tag
        in
        Element { tag_name; attributes; children })
      <* skip_ws 0)
;;

let parse s =
  let p =
    let* doctype_txt = optional doctype in
    let+ root = node in
    doctype_txt, root
  in
  P.parse_string p s
;;

module E = Easy_format

let element_style =
  { E.list with
    wrap_body = `Force_breaks
  ; space_after_opening = false
  ; space_before_closing = false
  ; separators_stick_left = false
  ; align_closing = true
  }
;;

let attribute_style =
  { E.list with
    wrap_body = `Never_wrap
  ; space_after_opening = false
  ; space_before_closing = false
  ; separators_stick_left = false
  ; align_closing = true
  }
;;

let rec format_node = function
  | Text txt -> E.Atom (txt, E.atom)
  | Element { tag_name; children; attributes } ->
    let attributes = List.map format_attribute attributes in
    let children = List.map format_node children in
    if List.length attributes = 0
    then
      E.List (("<" ^ tag_name ^ ">", "", "</" ^ tag_name ^ ">", element_style), children)
    else (
      let style = { element_style with wrap_body = `No_breaks } in
      let start_tag = E.List (("<" ^ tag_name ^ " ", "", ">", style), attributes) in
      let children = E.List (("", "", "", element_style), children) in
      let end_tag = E.Atom ("</" ^ tag_name ^ ">", E.atom) in
      E.List (("", "", "", element_style), [ start_tag; children; end_tag ]))
  | _ -> failwith "not implemented"

and format_attribute (Attr (attr_name, attr_val)) =
  let style = { element_style with wrap_body = `No_breaks } in
  let attr_val =
    match attr_val with
    | Some v -> E.Atom (v, E.atom)
    | None -> E.Atom ("", E.atom)
  in
  E.List
    (("", "", "", style), [ E.Atom (attr_name, E.atom); E.Atom ("=", E.atom); attr_val ])
;;

module F = Format

let pp fmt node =
  let ef = format_node node in
  E.Pretty.to_formatter fmt ef
;;
