open Core
open Async

open Alcotest
open Alcotest_async

let warp10 () =
  match Sys.(getenv "OVH_METRICS_URL",
             getenv "OVH_METRICS_TOKEN") with
  | Some uri, Some token ->
    let uri = Uri.of_string uri in
    let pt = Warp10.create_long
        ~labels:["label0", "val0" ; "label1", "val1"]
        ~name:"foo" 43L in
    Warp10_async.record ~uri ~token
      (Pipe.of_list [ pt ]) ;
    Clock_ns.(after @@ Time_ns.Span.of_int_sec 1)
  | _ -> failwith "Missing environment variables for metrics server"

let basic =
  "basic", [
    test_case "warp10" `Quick warp10 ;
  ]

let () =
  run "warp10" [
    basic
  ]
