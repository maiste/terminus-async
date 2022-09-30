let port = 55422

open Helpers

module Test =
  Generator.MakeTest
    (Terminus_async)
    (struct
      let port = port
    end)

let exec m =
  Async.Thread_safe.block_on_async_exn (fun () -> Server.with_server ~port m ())

let () = Test.run ~exec "Cohttp async"
