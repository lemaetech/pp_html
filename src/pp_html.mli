(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** Represents parsed HTML. *)
type t

(** [parse s] returns a parsed instance of [s] *)
val parse : string -> t

(** [pp ~indent fmt t] pretty prints [t] on a given formatter [fmt].

    {4:pp_examples Examples}

    {[
      let () =
        Pp_html.parse
          {|<!DOCTYPE html><html><body><br> <hr class="class1" id="id1"/><!-- This is a comment --> <div class="class1" id="id1">Hello World!</div><div></div></body></html>|}
        |> Pp_html.pp ~indent:4 Format.std_formatter
      ;;
    ]}

    The above outputs the following

    {v
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
    v} *)
val pp : ?indent:int -> Format.formatter -> t -> unit
