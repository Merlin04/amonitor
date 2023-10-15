open Client
open Args
open Arduino_cli.Board.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Monitor.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Port.Cc.Arduino.Cli.Commands.V1
open Lwt
open Lwt.Syntax
open LTerm_text

let get_time () =
  let tm = Unix.time () |> Unix.localtime in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let monitor ~conn ~instance ~term =
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
    let* () = LTerm.fprints term (match m with
      | { error = e; _ } when e <> "" -> eval [B_fg LTerm_style.red; S("--- ERROR: "); E_fg; S(e ^ "\n")]
      | { success = true; _ } -> (*"Port successfully opened\n"*) eval [ S("") ]
      | { rx_data = d; _ } ->
        let s = Bytes.to_string d in
        if not args.timestamps then eval [ S(s) ] else s
          |> String.split_on_char '\n'
          |> List.concat_map (fun l -> if String.length l = 0 then [] else [
               B_fg LTerm_style.lblack;
               S("(" ^ (get_time ()) ^ ")  ");
               E_fg;
               S(l ^ "\n");
             ])
          |> eval
        ) in
    after_print ()
  ) in
  let () = send (Some (MonitorRequest.make ~instance ~port ())) in
  let* () = LTerm.fprintls term (eval [
    B_fg Ui.trans_blue;
    S("Connecting to " ^ (List.assoc_opt "name" port.properties |> Option.value ~default:port.label));
    B_fg LTerm_style.lblack;
    S(" (" ^ port.address ^ ")");
    E_fg;
  ]) in

  (* exit when the input_promise resolves (interrupt), or when both call and iter are done (request closed and all responses have been handled *)
  Lwt.pick [input_promise; Lwt.join [call_promise; iter_promise]]