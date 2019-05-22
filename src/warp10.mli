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

val pp : Format.formatter -> t -> unit
val to_string : t -> string

val create :
  ?ts:Ptime.t ->
  ?coord:float * float ->
  ?elevation:int ->
  ?labels:(string * string) list ->
  name:string -> value -> t

val create_long :
  ?ts:Ptime.t ->
  ?coord:float * float ->
  ?elevation:int ->
  ?labels:(string * string) list ->
  name:string -> int64 -> t

val create_double :
  ?ts:Ptime.t ->
  ?coord:float * float ->
  ?elevation:int ->
  ?labels:(string * string) list ->
  name:string -> float -> t

val create_bool :
  ?ts:Ptime.t ->
  ?coord:float * float ->
  ?elevation:int ->
  ?labels:(string * string) list ->
  name:string -> bool -> t

val create_string :
  ?ts:Ptime.t ->
  ?coord:float * float ->
  ?elevation:int ->
  ?labels:(string * string) list ->
  name:string -> string -> t

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
