## Pretty-printer, formatter for HTML5

## Example
```ocaml 
let () =
  let html = 
    {|<!DOCTYPE html><html><body><br> <hr class="class1" id="id1"/><!-- This is a comment --> <div class="class1" id="id1">Hello World!       </div><div></div></body></html>|} in 
  Pp_html.parse html
  |> Pp_html.pp ~indent:4 Format.std_formatter
;;
```
The above formats the following HTML5.
```html
<!DOCTYPE html>

<html>
    <body>
        <br />
        <hr class="class1"
            id="id1"/>

        <!--  This is a comment  -->

        <div class="class1"
             id="id1">
            Hello World!
        <div>
        <div></div>
    </body>
</html>
```
