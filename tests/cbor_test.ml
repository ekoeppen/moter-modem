open Cmdliner

let major_test s =
  let major = Cbor.major_of_string s |> Cbor.string_of_major in
  Logs.info (fun m -> m "Major: %s" major)

let int_test s =
  match Cbor.int_of_string s with
  | Some (n, _) -> Logs.info (fun m -> m "Integer: %d" n)
  | None -> Logs.info (fun m -> m "Not an integer")

let int_2x_test s =
  let open Core.Option.Let_syntax in
  let%bind (n1, s) = Cbor.int_of_string s in
  let%map (n2, _) = Cbor.int_of_string s in
  Logs.info (fun m -> m "Integers: %d %d" n1 n2)

let float_test s =
  match Cbor.float_of_string s with
  | Some (n, _) -> Logs.info (fun m -> m "Float: %f" n)
  | None -> Logs.info (fun m -> m "No a float")

let tag_test s =
  match Cbor.tag_of_string s with
  | Some (n, _) -> Logs.info (fun m -> m "Tag: %d" n)
  | None -> Logs.info (fun m -> m "Not a tag")

let string_test s =
  match Cbor.byte_string_of_string s with
  | Some (str, _) -> Logs.info (fun m -> m "String: %s" str)
  | None -> Logs.info (fun m -> m "Not a string")

let test _ =
  major_test "\x01";
  major_test "\x21";
  major_test "\x41";
  major_test "\x61";
  major_test "\x81";
  major_test "\xa1";
  major_test "\xc1";
  major_test "\xe1";
  int_test "\x01";
  int_test "\x25";
  int_test "\xb1";
  let _ = int_2x_test "\x01\x02" in
  let _ = int_2x_test "\x18\x3f\x1a\x01\x01\x01\x01" in
  float_test "\xc4\x82\x21\x19\x6a\xb3";
  tag_test "\xc1";
  string_test "\x45Hello";
  ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let logging =
  let env = Arg.env_var "CBOR_TEST_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let cmd =
  let doc = "CBOR Test" in
  let exits = Term.default_exits in
  Term.(const test $ logging),
  Term.info "cbor_test" ~doc ~exits

let () = Term.(eval cmd |> exit)

