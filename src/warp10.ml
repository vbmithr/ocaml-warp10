(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type t = {
  meta : meta ;
  value : value ;
}

and meta = {
  ts: Ptime.t option ;
  coord: coord option ;
  elevation: int option ;
  name: string ;
  labels: (string * string) list ;
}

and coord = {
  lat: float ;
  lon: float ;
}

and value =
  | Int of int
  | Long of int64
  | Double of float
  | Bool of bool
  | String of string

let create ?ts ?coord ?elevation ?(labels=[]) ~name value =
  let coord =
    match coord with
    | None -> None
    | Some (lat, lon) -> Some { lat ; lon } in
  { meta = { ts ; coord ; elevation ; name ; labels } ; value }

let create_long ?ts ?coord ?elevation ?labels ~name value =
  create ?ts ?coord ?elevation ?labels ~name (Long value)
let create_double ?ts ?coord ?elevation ?labels ~name value =
  create ?ts ?coord ?elevation ?labels ~name (Double value)
let create_bool ?ts ?coord ?elevation ?labels ~name value =
  create ?ts ?coord ?elevation ?labels ~name (Bool value)
let create_string ?ts ?coord ?elevation ?labels ~name value =
  create ?ts ?coord ?elevation ?labels ~name (String value)

let pp_print_ts ppf ts =
  Format.fprintf ppf "%.0f" (Ptime.to_float_s ts /. 1000.)

let pp_print_coord ppf { lat ; lon } =
  Format.fprintf ppf "%f:%f" lat lon

let pp_print_bool ppf b =
  Format.pp_print_string ppf (if b then "T" else "F")

let pp_print_pct_encode ppf s =
  Format.pp_print_string ppf (Uri.pct_encode s)

let pp_print_option pp ppf = function
  | None -> Format.pp_print_string ppf ""
  | Some v -> Format.fprintf ppf "%a" pp v

let pp_print_comma_delimited pp ppf v =
  let open Format in
  pp_print_list ~pp_sep:(fun ppf () -> pp_print_char ppf ',') pp ppf v

let pp_print_label ppf (k, v) =
  Format.fprintf ppf "%a=%a" pp_print_pct_encode k pp_print_pct_encode v

let pp_print_value ppf = function
  | Int i -> Format.fprintf ppf "%d" i
  | Long i -> Format.fprintf ppf "%Ld" i
  | Double f -> Format.pp_print_float ppf f
  | Bool b -> pp_print_bool ppf b
  | String s -> Format.fprintf ppf "'%a'" pp_print_pct_encode s

let pp ppf ({ meta = { ts ; coord ; elevation ; name ; labels } ; value }) =
  Format.fprintf ppf "%a/%a/%a %a{%a} %a"
    (pp_print_option pp_print_ts) ts
    (pp_print_option pp_print_coord) coord
    (pp_print_option Format.pp_print_int) elevation
    pp_print_pct_encode name
    (pp_print_comma_delimited pp_print_label) labels
    pp_print_value value

let to_string v =
  Format.asprintf "%a" pp v

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
