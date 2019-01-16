(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async
open Cohttp_async

let record ~uri ~token vs =
  let headers =
    Cohttp.Header.of_list [
      "Content-Type", "text/plain" ;
      "X-Warp10-Token", token ;
    ] in
  let body = Body.of_pipe (Pipe.map vs ~f:(fun v ->
      Format.asprintf "%a@." Warp10.pp v)) in
  let rec loop () =
    Monitor.try_with_or_error begin fun () ->
      Client.post ~chunked:true ~headers ~body uri
    end >>= function
    | Error e ->
      if Pipe.is_closed vs then
        Deferred.Or_error.fail e
      else
        Clock_ns.(after @@ Time_ns.Span.of_int_sec 5) >>=
        loop
    | Ok _ -> Deferred.Or_error.return ()
  in
  loop ()

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
