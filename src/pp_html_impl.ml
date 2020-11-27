module P = Reparse.Parser
open Sexplib.Std
open P
open P.Infix

type t =
  | T of
      { doctype : string option
      ; root : node
      }

and node =
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

let sprintf = Printf.sprintf
let skip_ws at_least = P.(skip ~at_least (any [ htab; lf; char '\x0C'; cr; space ]))

let comments =
  take_between ~start:(string "<!--") ~end_:(string "-->") next
  >>= string_of_chars
  >|= fun s -> Comments s
;;

let doctype =
  optional
  @@ (take_between
        next
        ~start:(string ~case_sensitive:false "<!DOCTYPE" *> skip_ws 1)
        ~end_:(char '>')
     >>= string_of_chars)
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
        <?> sprintf "Invalid HTML tag"
      in
      let* attributes = attributes in
      let void = is_void tag_name in
      let* () =
        (if void then (optional @@ char '/') *> unit else unit) <* char '>' <* skip_ws 0
      in
      let closing_tag =
        string "</" *> string tag_name *> skip_ws 0 *> char '>'
        <?> sprintf "Closing tag '</%s>' missing." tag_name
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
    let* doctype = doctype in
    let+ root = node in
    T { doctype; root }
  in
  P.parse_string p s
;;

module E = Easy_format

let list_style =
  { E.list with
    wrap_body = `Force_breaks
  ; space_after_opening = false
  ; space_after_separator = false
  ; space_before_separator = false
  ; space_before_closing = false
  ; separators_stick_left = false
  ; align_closing = true
  }
;;

let atom' v = E.Atom (v, E.atom)

let rec format_node = function
  | Text txt -> atom' txt
  | Comments comments -> atom' @@ sprintf "<!-- %s -->" comments
  | Void { tag_name; attributes } ->
    if List.length attributes = 0
    then atom' @@ sprintf "<%s/>" tag_name
    else (
      let attributes' = List.map format_attribute attributes |> String.concat " " in
      atom' @@ sprintf "<%s %s/>" tag_name attributes')
  | Element { tag_name; children; attributes } ->
    let children = List.map format_node children in
    if List.length attributes = 0
    then
      E.List
        ((sprintf "<%s>" tag_name, "", sprintf "</%s>" tag_name, list_style), children)
    else (
      let attributes' = List.map format_attribute attributes |> String.concat " " in
      let open_tag = sprintf "<%s %s>" tag_name attributes' in
      let end_tag = sprintf "</%s>" tag_name in
      E.List ((open_tag, "", end_tag, list_style), children))

and format_attribute (Attr (attr_name, attr_val)) =
  match attr_val with
  | Some attr_val -> sprintf {|%s="%s"|} attr_name attr_val
  | None -> ""
;;

module F = Format

let pp fmt (T { doctype; root }) =
  let doc =
    match doctype with
    | Some s ->
      E.List
        ( ("", "", "", list_style)
        , [ atom' @@ sprintf "<!DOCTYPE %s>" s; atom' ""; format_node root ] )
    | None -> format_node root
  in
  E.Pretty.to_formatter fmt doc
;;
