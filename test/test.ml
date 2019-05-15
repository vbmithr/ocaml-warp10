open Core
open Async

open Alcotest
open Alcotest_async

let warp10 () =
  match Sys.getenv "OVH_METRICS_URL" with
  | None ->
    failwith "Missing env var OVH_METRICS_URL"
  | Some uri ->
    let uri = Uri.of_string uri in
    let pt = Warp10.create_long ~name:"foo" 54L in
    Warp10_async.record uri (Pipe.of_list [ pt ]) ;
    Clock_ns.(after @@ Time_ns.Span.of_int_sec 1)

let basic =
  "basic", [
    test_case "warp10" `Quick warp10 ;
  ]

let () =
  run "warp10" [
    basic
  ]
