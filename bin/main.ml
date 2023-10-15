open Client
open Arduino_cli.Commands.Cc.Arduino.Cli.Commands.V1
open Lwt
open Lwt.Syntax
open Args
open Monitor

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

    let* () = if args.ac_version then
      call_version ~conn >|= Result.get_ok >|= fst >>= LTerm.fprintl term
    else monitor ~conn ~instance ~term in

    daemon#kill Sys.sigterm;
    Lwt.return_unit
  )