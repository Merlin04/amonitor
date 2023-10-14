open Client
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1
open Arduino_cli.Board.Cc.Arduino.Cli.Commands.V1
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
    let* () = call_board_list ~conn ~req:(BoardListRequest.make ~instance ())
      >|= Result.get_ok >|= fst
      >|= BoardListResponse.show
      >>= Lwt_io.printl in

    daemon#kill Sys.sigterm;
    Lwt.return_unit
  )