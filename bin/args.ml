type config = {
  port : string Option.t;
  timestamps: bool;
  ac_version: bool;
}

let args = (
  let port_ref = ref None in
  let timestamps_ref = ref false in
  let ac_version_ref = ref false in
  let speclist = [
    ("-p", Arg.String (fun s -> port_ref := Some s), "  Set port to connect to (overrides auto-detection)");
    ("-t", Arg.Set timestamps_ref, "  Show timestamps next to received lines");
    ("-ac-version", Arg.Set ac_version_ref, "  Show arduino-cli version and exit")
  ] in
  let usage_msg = "amonitor [-p port] [-t] [-ac-version]" in

  Arg.parse speclist (fun _ -> ()) usage_msg;
  {
    port = !port_ref;
    timestamps = !timestamps_ref;
    ac_version = !ac_version_ref
  }
)