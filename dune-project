(lang dune 3.10)

(name amonitor)

(generate_opam_files true)

(source
 (github Merlin04/amonitor))

(authors "Merlin04")

(maintainers "Merlin04")

(license LICENSE)

(documentation https://github.com/Merlin04/amonitor)

(package
 (name amonitor)
 (synopsis "Improved Arduino serial monitor")
 (description "An improved Arduino serial monitor that connects to the core Arduino service and provides niceties not found in the arduino-cli built-in serial monitor.")
 (depends ocaml dune (grpc-lwt (>= 0.1.0)) (h2-lwt-unix (>= 0.9.0)) (ocaml-protoc-plugin (>= 4.4.0)) ppx_deriving lambda-term)
 (tags
  (topics serial)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
