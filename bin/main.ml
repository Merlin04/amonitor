open Client
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1
open Lwt.Infix
open Lwt.Syntax

let () =
  let port = 50051 in
  let address = "localhost" in
  let req = CreateRequest.make () in
  Lwt_main.run (
    let* connection = client address port in
    let* instance = call_create connection req >|= Result.get_ok >|= fst >|= Option.get in
    let* () = call_init connection (InitRequest.make ~instance ()) in
    let* () = call_version connection >|= Result.get_ok >|= fst >>= Lwt_io.printl in
    Lwt.return_unit
  )