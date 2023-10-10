open Grpc_lwt
open Lwt.Syntax
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1

let call_server address port req =
  (* Setup Http/2 connection *)
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  let* connection =
    H2_lwt_unix.Client.create_connection ~error_handler socket
  in

  (* codegen *)
  let open Ocaml_protoc_plugin in
  let encode, decode = Service.make_client_functions ArduinoCoreService.create in
  let enc = encode req |> Writer.contents in

  Client.call ~service:"cc.arduino.cli.commands.v1.ArduinoCoreService" ~rpc:"Create"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary enc ~f:(fun decoder ->
        let+ decoder = decoder in
        match decoder with
          | Some decoder -> (
            Reader.create decoder |> decode |> function
              | Ok v -> v
              | Error e ->
                failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e)))
          | None -> ArduinoCoreService.Create.Response.make ()))
  ()