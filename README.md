## Pretty-printer, formatter for HTML5 (Unreleased and not production ready)

Note - The parser hasn't been validated against HTML5 RFC.

## Example
```ocaml 
let () =
  let html = 
    {|<!DOCTYPE html><html><body><br> 
    <hr class="class1" id="id1"/>
    <!-- This is a comment --> 
     <div class="class1"     id ='id1' style="align: center" enabled>Hello World!</div>
     <div></div>
     <div disabled id   =   hello id = 
      hello2 id3 = 
       hello3></div>
    </body></html>|} in 
  Pp_html.parse html
  |> Pp_html.pp ~indent:4 Format.std_formatter
;;
```
The above code produces the following formatted HTML5.
```html
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
</html>
```
