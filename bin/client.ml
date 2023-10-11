open Grpc_lwt
open Lwt.Syntax
open Ocaml_protoc_plugin
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1

let client address port : H2_lwt_unix.Client.t Lwt.t =
  (* Setup Http/2 connection *)
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  H2_lwt_unix.Client.create_connection ~error_handler socket

let core_call conn rpc handler =
  Client.call ~service:"cc.arduino.cli.commands.v1.ArduinoCoreService" ~rpc:rpc
    ~do_request:(H2_lwt_unix.Client.request conn ~error_handler:ignore)
    ~handler:handler
    ()

let call_create conn req =
  let encode, decode = Service.make_client_functions ArduinoCoreService.create in
  let enc = encode req |> Writer.contents in
  core_call conn "Create" (Client.Rpc.unary enc ~f:(fun decoder ->
    let+ decoder = decoder in
    match decoder with
      | Some decoder -> (
        Reader.create decoder |> decode |> function
          | Ok v -> v
          | Error e ->
            failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e)))
      | None -> failwith "Failed to create instance"))

let call_init conn req =
  let encode, decode = Service.make_client_functions ArduinoCoreService.init in
  let enc = encode req |> Writer.contents in
  let* _stream = core_call conn "Init" (Client.Rpc.server_streaming enc ~f:(fun responses ->
    let stream = Lwt_stream.map (fun str ->
      Reader.create str |> decode |> function
        | Ok v -> print_endline (InitResponse.show v); v
        | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
    ) responses in Lwt_stream.to_list stream
  ))
  in
  Lwt.return_unit

let call_version conn =
  let encode, decode = Service.make_client_functions ArduinoCoreService.version in
  let enc = encode (VersionRequest.make ()) |> Writer.contents in
  core_call conn "Version" (Client.Rpc.unary enc ~f:(fun decoder ->
    let+ decoder = decoder in
    match decoder with
      | Some decoder -> (
        Reader.create decoder |> decode |> function
          | Ok v -> v
          | Error e ->
            failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e)))
      | None -> failwith "Failed to get version"))

