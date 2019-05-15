(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async
open Cohttp_async

let src = Logs.Src.create ~doc:"Warp10 - Async" "warp10.async"
module Log = (val Logs_async.src_log src : Logs_async.LOG)

let record uri vs =
  let token = match Uri.user uri with
    | None -> invalid_arg "missing token in URL"
    | Some token -> token in
  let uri = Uri.with_userinfo uri None in
  let headers =
    Cohttp.Header.of_list [
      "Content-Type", "text/plain" ;
      "X-Warp10-Token", token ;
    ] in
  let body = Body.of_pipe (Pipe.map vs ~f:(fun v ->
      Format.asprintf "%a@." Warp10.pp v)) in
  let interrupt =
    Pipe.closed vs >>= fun () ->
    let rec loop () =
      if Pipe.is_empty vs then Deferred.unit
      else Clock_ns.after @@ Time_ns.Span.of_int_sec 5 >>= loop
    in
    loop ()
  in
  let rec loop () =
    Monitor.try_with_or_error begin fun () ->
      Client.post ~interrupt ~chunked:true ~headers ~body uri
    end >>= fun e ->
    if Pipe.is_closed vs then
      Log.info (fun m -> m "Input metrics pipe is closed, aborting")
    else begin match e with
      | Error e ->
        Log.err (fun m -> m "%a" Error.pp e)
      | Ok _ -> Deferred.unit
    end >>= fun () ->
      Clock_ns.(after @@ Time_ns.Span.of_int_sec 5) >>=
      loop
  in
  don't_wait_for (loop ())

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
