open Pp_html_impl

let () = Printexc.record_backtrace false

let pp_sexp =
  let open Sexp_pretty in
  let config = Config.update ~color:false Config.default in
  Sexp_pretty.(pp_formatter config Format.std_formatter)
;;

let test_ast s =
  let _, node = parse s in
  pp_sexp @@ sexp_of_node node
;;

let%expect_test "text, nodes, comments, children" =
  test_ast {|<html><body><div>Hello World!</div></body></html>|};
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
          (attributes ())
          (children ((Text "Hello World!")))))))))) |}]
;;
