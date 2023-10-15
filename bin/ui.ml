open Lwt
open Lwt.Syntax
open React

let trans_pink = LTerm_style.rgb 245 169 184
let trans_blue = LTerm_style.rgb 91 206 250

class read_line ~term ~history = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (S.const (LTerm_text.(eval [
      B_fg trans_pink;
      S ("> ");
      E_fg;
    ])))
end

let input_loop ~term ~handler =
  let history = LTerm_history.create [] in
  let rl_ref = ref None in
  let rec loop () =
    Lwt.catch
      (fun () ->
        rl_ref := Some (new read_line ~term ~history:(LTerm_history.contents history));
        (Option.get (!rl_ref))#run >|= fun line -> rl_ref := None; Some line)
      (function
        | Sys.Break -> rl_ref := None; return None
        | exn -> Lwt.fail exn)
    >>= function
      | Some line ->
        let* () = line |> Zed_string.to_utf8 |> handler in
        LTerm_history.add history line; loop ()
      | None -> LTerm.fprints term LTerm_text.(eval [B_fg trans_pink; S("  ^C\n"); E_fg]) >>= loop in
  (loop (),
   (fun () -> if !rl_ref = None then Lwt.return_unit else
      let* () = LTerm.clear_line term in
      LTerm.move term 0 (-99999) (* hack: let's hope no one has a terminal with 10000 cols *)),
   (fun () -> if !rl_ref = None then Lwt.return_unit else (Option.get (!rl_ref))#draw_update))