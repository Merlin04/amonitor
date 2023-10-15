type config = {
  port : string Option.t;
  timestamps: bool;
}

let args = (
  let port_ref = ref None in
  let timestamps_ref = ref false in
  let speclist = [
    ("-p", Arg.String (fun s -> port_ref := Some s), "  Set port to connect to (overrides auto-detection)");
    ("-t", Arg.Set timestamps_ref, "  Show timestamps next to received lines")
  ] in
  let usage_msg = "amonitor [-p port]" in

  Arg.parse speclist (fun _ -> ()) usage_msg;
  {
    port = !port_ref;
    timestamps = !timestamps_ref
  }
)