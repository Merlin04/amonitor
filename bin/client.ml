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

let unwrap_decode_res = function
  | Ok v -> v
  | Error e -> failwith (Printf.sprintf "Could not decode response: %s" (Result.show_error e))

let call_unary (type t_req) (type t_rep) (reqrep : (t_req, t_rep) reqrep) rpc ~conn ~req =
  let encode, decode = Service.make_client_functions reqrep in
  let enc = encode req |> Writer.contents in
  core_call conn rpc (Client.Rpc.unary enc ~f:(fun decoder ->
    decoder >|= Option.get >|= Reader.create >|= decode >|= unwrap_decode_res
  ))

let decode_stream (decode : Reader.t -> 'a Result.t) s : 'a Lwt_stream.t =
  Lwt_stream.map (fun r -> r |> Reader.create |> decode |> unwrap_decode_res) s

(* blocking - promise doesn't resolve until network request closes *)
let call_server_stream (type t_req) (type t_rep) (reqrep : (t_req, t_rep) reqrep) rpc ~conn ~req =
  let encode, decode = Service.make_client_functions reqrep in
  let enc = encode req |> Writer.contents in
  core_call conn rpc (Client.Rpc.server_streaming enc ~f:(fun responses ->
    responses |> decode_stream decode |> Lwt.return
  ))

(* non-blocking - returns send/responses in addition to promise that resolves when request closes *)
let call_bidi_stream_nb (type t_req) (type t_rep) (reqrep : (t_req, t_rep) reqrep) rpc ~conn =
  let encode, decode = Service.make_client_functions reqrep in
  let (p, res) = Lwt.wait () in
  let call_p = core_call conn rpc (Client.Rpc.bidirectional_streaming ~f:(fun send responses ->
    ((fun (v : t_req Option.t) -> v |> Option.map (fun r -> encode r |> Writer.contents) |> send),
      decode_stream decode responses) |> Lwt.wakeup_later res; Lwt.return_unit
  )) in
  let* (send, responses) = p in
  (send, responses, call_p) |> Lwt.return

let call_create = call_unary ArduinoCoreService.create "Create" ~req:(CreateRequest.make ())
let call_init = call_server_stream ArduinoCoreService.init "Init"
let call_version = call_unary ArduinoCoreService.version "Version" ~req:(VersionRequest.make ())
let call_board_list = call_unary ArduinoCoreService.boardList "BoardList"
let call_monitor = call_bidi_stream_nb ArduinoCoreService.monitor "Monitor"