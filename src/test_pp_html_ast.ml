open Pp_html_impl

let () = Printexc.record_backtrace false

let pp_sexp =
  let open Sexp_pretty in
  let config = Config.update ~color:false Config.default in
  Sexp_pretty.(pp_formatter config Format.std_formatter)
;;

let test_ast s =
  let (T { root; _ }) = parse s in
  pp_sexp @@ sexp_of_node root
;;

let test_pp s =
  let doc = parse s in
  pp Format.std_formatter doc
;;

let%expect_test "text, nodes, comments, children" =
  test_ast {|<html><body><div class="class1" id="id1">Hello World!</div></body></html>|};
  [%expect
    {|
    (Element
      (tag_name html)
      (attributes ())
      (children ((
        Element
        (tag_name body)
        (attributes ())
        (children ((
          Element
          (tag_name div)
          (attributes (
            (Attr (class (class1)))
            (Attr (id    (id1)))))
          (children ((Text "Hello World!")))))))))) |}]
;;

let%expect_test "text, nodes, comments, children " =
  test_pp
    {|<!DOCTYPE html><html><body><br> <hr /><div class="class1" id="id1">Hello World!</div></body></html>|};
  [%expect
    {|
    <!DOCTYPE html>
    <html>
      <body>
        <br/>
        <hr/>
        <div class="class1" id="id1">
          Hello World!
        </div>
      </body>
    </html> |}]
;;
