open Client
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Board.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Monitor.Cc.Arduino.Cli.Commands.V1
open Lwt.Infix
open Lwt.Syntax

let () =
  Lwt_main.run (
    let daemon_cmd = Lwt_process.shell "arduino-cli daemon" in
    let daemon = Lwt_process.open_process_in daemon_cmd in
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
      | r -> r |> Lwt_list.iter_s (fun r -> r |> InitResponse.show |> Lwt_io.printl) >>= failwith "Unexpected output from init" in
    let* () = call_version ~conn >|= Result.get_ok >|= fst >>= Lwt_io.printl in
    let* port = call_board_list ~conn ~req:(BoardListRequest.make ~instance ())
      >|= Result.get_ok >|= fst
      >|= fun ports -> (List.find_map (fun (b : DetectedPort.t) -> match b.matching_boards with
        | _ :: _ -> b.port
        | _ -> None) ports
      |> function
        | Some p -> p
        | None -> failwith "Could not find a matching board.") in
    let* m_call = call_monitor ~conn in
    let (send, responses, p) = m_call in
    let p_handler = p >|= Result.get_ok >|= fst in
    let iter_promise = responses |> Lwt_stream.iter_s (fun (m : MonitorResponse.t) ->
      Lwt_io.print (match m with
         | { error = e; _ } when e <> "" -> "--- ERROR: " ^ e ^ "\n"
         | { success = true; _ } -> "Port successfully opened\n"
         | { rx_data = d; _ } -> Bytes.to_string d)
    ) in
    let () = send (Some (MonitorRequest.make ~instance ~port ())) in
    let* () = Lwt_io.printlf "Connecting to %s (%s)" (List.assoc_opt "name" port.properties |> Option.value ~default:port.label) port.address in
    let* () = Lwt.join [p_handler; iter_promise] in

    daemon#kill Sys.sigterm;
    Lwt.return_unit
  )