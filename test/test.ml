open Core
open Async
open Alcotest_async

let warp10 () =
  match Sys.getenv "OVH_METRICS_URL" with
  | None -> Deferred.unit
  | Some uri ->
    let uri = Uri.of_string uri in
    let pt = Warp10.create_long ~name:"foo" 54L in
    Warp10_async.record uri (Pipe.of_list [ pt ])

let basic =
  "basic", [
    test_case "warp10" `Quick warp10 ;
  ]

let main () =
  run "warp10" [
    basic
  ]

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())
