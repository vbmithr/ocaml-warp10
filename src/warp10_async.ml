(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async
open Httpaf

let src = Logs.Src.create ~doc:"Warp10 - Async" "warp10.async"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let record uri vs =
  let token = match Uri.user uri with
    | None -> invalid_arg "missing token in URL"
    | Some token -> token in
  let uri = Uri.with_userinfo uri None in
  let headers =
    Headers.of_list [
      "Content-Type", "text/plain" ;
      "X-Warp10-Token", token ;
    ] in
  let buf = Buffer.create 13 in
  Pipe.iter' ~continue_on_error:true vs ~f:begin fun msgq ->
    Buffer.clear buf ;
    Queue.iter msgq ~f:begin fun msg ->
      Buffer.add_string buf (Format.asprintf "%a@." Warp10.pp msg)
    end ;
    Monitor.try_with begin fun () ->
      Fastrest.simple_call
        ~headers
        ~body:(Buffer.contents buf)
        ~meth:`POST uri >>= fun (resp, _body) ->
      if not (Status.is_successful resp.status) then
        failwith (Status.to_string resp.status)
      else Deferred.unit
    end >>= function
    | Error e -> Log_async.err (fun m -> m "%a" Exn.pp e)
    | Ok () -> Deferred.unit
  end

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
