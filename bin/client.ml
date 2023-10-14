open Grpc_lwt
open Lwt
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

type ('req, 'rep) reqrep = (module Ocaml_protoc_plugin.Service.Message with type t = 'req) *
                        (module Ocaml_protoc_plugin.Service.Message with type t = 'rep)

let call_unary (type t_req) (type t_rep) (reqrep : (t_req, t_rep) reqrep) rpc ~conn ~req =
  let encode, decode = Service.make_client_functions reqrep in
  let enc = encode req |> Writer.contents in
  core_call conn rpc (Client.Rpc.unary enc ~f:(fun decoder ->
    decoder >|= Option.get >|= Reader.create >|= decode >|= function
      | Ok v -> v
      | Error e -> failwith (Printf.sprintf "Could not decode response: %s" (Result.show_error e))
  ))

let call_stream (type t_req) (type t_rep) (reqrep : (t_req, t_rep) reqrep) rpc ~conn ~req =
  let encode, decode = Service.make_client_functions reqrep in
  let enc = encode req |> Writer.contents in
  core_call conn rpc (Client.Rpc.server_streaming enc ~f:(fun responses ->
    Lwt_stream.map (fun r -> Reader.create r |> decode |> function
      | Ok v -> v
      | Error e -> failwith (Printf.sprintf "Could not decode response: %s" (Result.show_error e))
    ) responses |> Lwt.return
  ))

let call_create = call_unary ArduinoCoreService.create "Create" ~req:(CreateRequest.make ())
let call_init = call_stream ArduinoCoreService.init "Init"
let call_version = call_unary ArduinoCoreService.version "Version" ~req:(VersionRequest.make ())
let call_board_list = call_unary ArduinoCoreService.boardList "BoardList"