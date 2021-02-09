let () = Printexc.record_backtrace false

let pp txt =
  let html = Pp_html.parse txt in
  let config = Sexp_pretty.Config.(update ~color:false default) in
  Pp_html.sexp_of_t html |> Sexp_pretty.pp_formatter config Format.std_formatter
;;

let%expect_test "attribute: unquoted" =
  pp {|<div attr3=att3val></div>|};
  [%expect
    {|
    (T
      (doctype ())
      (root (
        Element
        (tag_name div)
        (attributes ((Attr (attr3 (att3val)))))
        (children ())))) |}]
;;

let%expect_test "attribute: double quoted" =
  pp {| <div attr3="att3val"></div> |};
  [%expect
    {|
    (T
      (doctype ())
      (root (
        Element
        (tag_name div)
        (attributes ((Attr (attr3 (att3val)))))
        (children ())))) |}]
;;

let%expect_test "attribute: single quoted" =
  pp {| <div attr3='val3'></div>|};
  [%expect
    {|
    (T
      (doctype ())
      (root (
        Element (tag_name div) (attributes ((Attr (attr3 (val3))))) (children ())))) |}]
;;

let%expect_test "attributes: double quoted, single quoted and unquoted" =
  pp {| <div attr3 = att3val class="class1" id = 'id1'></div>|};
  [%expect
    {|
    (T
      (doctype ())
      (root (
        Element
        (tag_name div)
        (attributes (
          (Attr (attr3 (att3val)))
          (Attr (class (class1)))
          (Attr (id    (id1)))))
        (children ())))) |}]
;;

let test_pp s =
  let doc = Pp_html.parse s in
  Pp_html.pp ~indent:4 Format.std_formatter doc
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
hello3><div>content<div>inner<span>asdfasdfasdfasdfasdfasdfasdfasdf
</span></div></div></div>
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
                <div>
                    content
                    <div>
                        inner
                        <span>
                            asdfasdfasdfasdfasdfasdfasdfasdf

                        </span>
                    </div>
                </div>
            </div>
        </body>
    </html> |}]
;;
