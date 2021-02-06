(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

open Reparse
open Infix
open Sexplib.Std

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
let ws = any [ htab; lf; char '\x0C'; cr; space ]
let skip_ws at_least = skip ~at_least ws

let comments =
  take_between ~start:(string "<!--") ~end_:(string "-->") next
  >>= string_of_chars
  >>| fun s -> Comments s
;;

let doctype =
  take_between
    next
    ~start:(string ~case_sensitive:false "<!DOCTYPE" *> skip_ws 1)
    ~end_:(char '>')
  >>= string_of_chars
  |> optional
;;

let text =
  let* txt = take_while next ~while_:(is_not @@ char '<') >>= string_of_chars in
  if String.length txt > 0 then return @@ Text txt else fail "Invalid HTML text node"
;;

let attribute =
  let illegal_attr_name_char =
    char_if (function
        | '\x00' .. '\x1F' | '\x7F' .. '\x9F' | ' ' | '"' | '\'' | '>' | '/' | '=' -> true
        | _ -> false)
  in
  let* () = skip_ws 1 *> unit in
  let* attr_name =
    let* name = take_while next ~while_:(is_not illegal_attr_name_char) in
    if List.length name = 0
    then fail "Attribute name is required!"
    else string_of_chars name
  in
  let* has_equal = is (skip_ws 0 *> char '=') in
  let+ attr_val =
    if has_equal
    then (
      let quoted_attr_val =
        let* c = any [ char '"'; char '\'' ] in
        take_while next ~while_:(is_not @@ char c) <* char c >>= string_of_chars
      in
      let unquoted_attr_val =
        let excluded_unquoted_attr_val_char =
          any [ ws; char '"'; char '\''; char '='; char '<'; char '>'; char '`' ]
        in
        let* attr_val =
          take_while next ~while_:(is_not excluded_unquoted_attr_val_char)
        in
        if List.length attr_val = 0
        then fail "Unquoted attribute value can't be an empty string."
        else string_of_chars attr_val
      in
      skip_ws 0 *> char '=' *> skip_ws 0 *> any [ quoted_attr_val; unquoted_attr_val ]
      >>| Option.some)
    else return None
  in
  Attr (attr_name, attr_val)
;;

let attributes = take attribute

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
      then return @@ Void { tag_name; attributes }
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
  parse_string p s
;;

module F = Format

let pp_attributes fmt attributes =
  let pp_attribute fmt (Attr (attr_name, attr_val)) =
    match attr_val with
    | Some v -> F.fprintf fmt {| %s="%s"|} attr_name v
    | None -> F.fprintf fmt " %s" attr_name
  in
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
    | Comments comments -> F.fprintf fmt "@;<!-- %s -->" comments
    | Void { tag_name; attributes = [] } -> F.fprintf fmt "<%s />" tag_name
    | Void { tag_name; attributes } ->
      F.open_vbox indent;
      F.fprintf fmt "<%s" tag_name;
      pp_attributes fmt attributes;
      F.fprintf fmt "/>";
      F.close_box ()
    | Element { tag_name; attributes; children } ->
      let pp_children = F.pp_print_list pp_node in
      F.open_vbox indent;
      F.fprintf fmt "<%s" tag_name;
      if List.length attributes > 0 then F.fprintf fmt "%a" pp_attributes attributes;
      F.fprintf fmt ">";
      if List.length children > 0 then F.fprintf fmt "@,%a" pp_children children;
      F.print_break 0 (-indent);
      F.fprintf fmt "</%s>" tag_name;
      F.close_box ()
  in
  F.open_vbox 0;
  pp_doctype fmt doctype;
  F.print_break 0 0;
  F.print_break 0 0;
  pp_node fmt root;
  F.close_box ()
;;

(* ---- Unit tests ---- *)

let () = Printexc.record_backtrace false

let pp_ast sexp =
  let open Sexp_pretty in
  let config = Config.update ~color:false Config.default in
  pp_formatter config Format.std_formatter sexp
;;

let%expect_test "attribute: unquoted" =
  parse_string attribute {| attr3=att3val|} |> sexp_of_attribute |> pp_ast;
  [%expect {|
    (Attr (attr3 (att3val))) |}]
;;

let%expect_test "attribute: double quoted" =
  parse_string attribute {| attr3="att3val"|} |> sexp_of_attribute |> pp_ast;
  [%expect {|
    (Attr (attr3 (att3val))) |}]
;;

let%expect_test "attribute: single quoted" =
  parse_string attribute {| attr3='val3'|} |> sexp_of_attribute |> pp_ast;
  [%expect {|
    (Attr (attr3 (val3))) |}]
;;

let%expect_test "attributes: double quoted, single quoted and unquoted" =
  parse_string attributes {| attr3 = att3val class="class1" id = 'id1'    |}
  |> sexp_of_list sexp_of_attribute
  |> pp_ast;
  [%expect
    {|
    ((Attr (attr3 (att3val)))
     (Attr (class (class1)))
     (Attr (id    (id1)))) |}]
;;

let test_pp s =
  let doc = parse s in
  pp ~indent:4 Format.std_formatter doc
;;

let%expect_test "PPrint: text, nodes, comments, children " =
  test_pp
    {|<!DOCTYPE html><html><body><br> 
    <hr class="class1" id="id1"/>
    <!-- This is a comment --> 
     <div class="class1"     id ='id1' style="align: center" enabled>Hello World!</div>
     <div></div>
     <div disabled id   =   hello id = 
      hello2 id3 = 
       hello3></div>
    </body></html>|};
  [%expect
    {|
    <!DOCTYPE html>

    <html>
        <body>
            <br />
            <hr class="class1"
                id="id1"/>

            <!--  This is a comment  -->
            <div class="class1"
                 id="id1"
                 style="align: center"
                 enabled>
                Hello World!
            </div>
            <div>
            </div>
            <div disabled
                 id="hello"
                 id="hello2"
                 id3="hello3">
            </div>
        </body>
    </html> |}]
;;
