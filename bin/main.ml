open Client
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1

let () =
  let open Lwt.Syntax in
  let port = 50051 in
  let address = "localhost" in
  let req = CreateRequest.make () in
  Lwt_main.run
    (let+ res = call_server address port req in
      match res with
        | Ok (res, _) -> print_endline (CreateResponse.show res)
        | Error _ -> print_endline "Error")