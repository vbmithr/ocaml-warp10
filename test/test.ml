open Core
open Async

open Alcotest
open Alcotest_async

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let warp10 () =
  match Sys.getenv "OVH_METRICS_URL" with
  | None ->
    failwith "Missing env var OVH_METRICS_URL"
  | Some uri ->
    let uri = Uri.of_string uri in
    let pt = Warp10.create_long ~name:"foo" 54L in
    Warp10_async.record uri (Pipe.of_list [ pt ])

let basic =
  "basic", [
    test_case "warp10" `Quick warp10 ;
  ]

let () =
  run "warp10" [
    basic
  ]
