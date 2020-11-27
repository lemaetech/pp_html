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

module F = Format

let pp_attribute fmt (Attr (attr_name, attr_val)) =
  match attr_val with
  | Some v -> F.fprintf fmt {| %s="%s"|} attr_name v
  | None -> F.fprintf fmt " %s" attr_name
;;

let pp_attributes fmt attributes =
  let pp_attributes = F.pp_print_list pp_attribute in
  F.open_vbox 0;
  F.fprintf fmt "%a" pp_attributes attributes;
  F.close_box ()
;;

let pp_doctype fmt = function
  | Some txt -> F.fprintf fmt "<!DOCTYPE %s>" txt
  | None -> ()
;;

let pp ?(indent = 2) fmt (T { doctype; root }) =
  let rec pp_node fmt = function
    | Text txt -> F.fprintf fmt "%s" txt
    | Comments comments -> F.fprintf fmt "@;<!-- %s -->@;" comments
    | Void { tag_name; attributes = [] } -> F.fprintf fmt "<%s />" tag_name
    | Void { tag_name; attributes } ->
      F.open_vbox indent;
      F.fprintf fmt "<%s" tag_name;
      pp_attributes fmt attributes;
      F.fprintf fmt "/>";
      F.close_box ()
    | Element { tag_name; attributes = []; children = [] } ->
      F.fprintf fmt "@[<h><%s></%s>@]" tag_name tag_name
    | Element { tag_name; attributes = []; children } ->
      let pp_children = F.pp_print_list pp_node in
      F.open_vbox indent;
      F.fprintf fmt "<%s>@,%a" tag_name pp_children children;
      F.print_break 0 (-indent);
      F.fprintf fmt "</%s>" tag_name;
      F.close_box ()
    | Element { tag_name; attributes; children } ->
      let pp_children = F.pp_print_list pp_node in
      F.open_vbox indent;
      F.fprintf fmt "<%s%a>@,%a" tag_name pp_attributes attributes pp_children children;
      F.print_break 0 (-indent);
      F.fprintf fmt "<%s>" tag_name;
      F.close_box ()
  in
  F.open_vbox 0;
  pp_doctype fmt doctype;
  F.print_break 0 0;
  F.print_break 0 0;
  pp_node fmt root;
  F.close_box ()
;;
