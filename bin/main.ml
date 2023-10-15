open Client
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Board.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Monitor.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Port.Cc.Arduino.Cli.Commands.V1
open Lwt.Infix
open Lwt.Syntax
open Args

let get_time () =
  let tm = Unix.time () |> Unix.localtime in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let ensure_newline s =
  if String.ends_with ~suffix:"\n" s then s else s ^ "\n"

let () =
  Lwt_main.run (
    let* () = LTerm_inputrc.load () in
    let* term = Lazy.force LTerm.stdout in

    let daemon_cmd = Lwt_process.shell "arduino-cli daemon" in
    let daemon = Lwt_process.open_process daemon_cmd in
    let* daemon_addr_line = Lwt_io.read_line daemon#stdout in
    let (address, port) = daemon_addr_line
      |> String.split_on_char ' '
      |> List.rev
      |> List.hd
      |> String.split_on_char ':'
      |> (function
            | address :: port :: [] -> (address, int_of_string port)
            | _ -> failwith "Unexpected output from arduino-cli daemon") in

    let* conn = client address port in
    let* instance = call_create ~conn >|= Result.get_ok >|= fst >|= Option.get in
    let* () = call_init ~conn ~req:(InitRequest.make ~instance ())
     >|= Result.get_ok >|= fst
     >>= Lwt_stream.to_list >>= function
      | [] -> Lwt.return_unit
      | r -> r |> Lwt_list.iter_s (fun r -> r |> InitResponse.show |> LTerm.fprintl term) >>= failwith "Unexpected output from init" in

(*    let* () = call_version ~conn >|= Result.get_ok >|= fst >>= LTerm.fprintl term in *)

    let* port = match args.port with
      | Some p -> Port.make ~address:p ~label:"port" ~protocol:"serial" ~protocol_label:"Serial Port" ~properties:[] ~hardware_id:"" () |> Lwt.return
      | None -> call_board_list ~conn ~req:(BoardListRequest.make ~instance ())
          >|= Result.get_ok >|= fst
          >|= fun ports -> (List.find_map (fun (b : DetectedPort.t) -> match b.matching_boards with
            | _ :: _ -> b.port
            | _ -> None) ports
          |> function
            | Some p -> p
            | None -> failwith "Could not find a connected board, and port was not specified.") in

    let* m_call = call_monitor ~conn in
    let (send, responses, p) = m_call in
    let call_promise = p >|= Result.get_ok >|= fst in

    let (input_loop_promise, before_print, after_print) =
      Ui.input_loop ~term ~handler:(fun line ->
        send (Some (MonitorRequest.make ~instance ~tx_data:(Bytes.of_string line) ()));
        Lwt.return_unit
      ) in

    let input_promise = Lwt.catch
      (fun () -> input_loop_promise)
      (function
        | LTerm_read_line.Interrupt -> Lwt.return_unit
        | exn -> Lwt.fail exn) in

    let iter_promise = responses |> Lwt_stream.iter_s (fun (m : MonitorResponse.t) ->
      let* () = before_print () in
      let* () = LTerm.fprint term (match m with
        | { error = e; _ } when e <> "" -> "--- ERROR: " ^ e ^ "\n"
        | { success = true; _ } -> (*"Port successfully opened\n"*) ""
        | { rx_data = d; _ } ->
          let s = Bytes.to_string d in
          if not args.timestamps then s else
          "(" ^ (get_time ()) ^ ")  " ^ (ensure_newline s)
          ) in
      after_print ()
    ) in
    let () = send (Some (MonitorRequest.make ~instance ~port ())) in
    let* () = LTerm.fprintlf term "Connecting to %s (%s)" (List.assoc_opt "name" port.properties |> Option.value ~default:port.label) port.address in

    (* exit when the input_promise resolves (interrupt), or when both call and iter are done (request closed and all responses have been handled *)
    let* () = Lwt.pick [input_promise; Lwt.join [call_promise; iter_promise]] in

    daemon#kill Sys.sigterm;
    Lwt.return_unit
  )