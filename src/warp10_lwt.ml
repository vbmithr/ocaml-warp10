(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Cohttp_lwt
open Cohttp_lwt_unix

let src = Logs.Src.create ~doc:"Warp10 - Lwt" "warp10.lwt"

let record uri vs =
  let token =
    match Uri.user uri with
    | None -> invalid_arg "no token in url"
    | Some token -> token in
  let headers =
    Cohttp.Header.of_list [
      "Content-Type", "text/plain" ;
      "X-Warp10-Token", token ;
    ] in
  let body = Body.of_stream (Lwt_stream.map (fun v ->
      Format.asprintf "%a@." Warp10.pp v) vs) in
  let rec loop () =
    Lwt.catch
      begin fun () ->
        Client.post ~chunked:true ~headers ~body uri >>= fun _ ->
        Lwt_stream.is_empty vs >>= function
        | true when Lwt_stream.is_closed vs -> Lwt.return_unit
        | _ -> Lwt_unix.sleep 5. >>= loop
      end
      begin fun exn ->
        Lwt_stream.is_empty vs >>= function
        | true when Lwt_stream.is_closed vs ->
          Logs_lwt.info ~src begin fun m ->
            m "Input metrics pipe is closed and empty, aborting"
          end
        | _ ->
          Logs_lwt.err ~src
            (fun m -> m "%s" (Printexc.to_string exn)) >>= fun () ->
          Lwt_unix.sleep 5. >>= loop
      end
  in
  Lwt.async loop

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
