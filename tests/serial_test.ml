open Cmdliner

let dle = '\x10'
let stx = '\x02'
let etx = '\x03'

let test _ device =
  let b = Bytes.create 80 in
  let f = Unix.openfile device [Unix.O_RDWR] 0644 in
  Logs.info (fun m -> m "Device %s opened" device);
  let ic = Unix.in_channel_of_descr f in
  let oc = Unix.out_channel_of_descr f in
  output_string oc "Hello\r\n"; flush oc;
  let n = input ic b 0 80 in
  Logs.info (fun m -> m "Read %d bytes" n);
  let n = input ic b 0 80 in
  Logs.info (fun m -> m "Read %d bytes" n);
  Unix.close f

let rec wait_for_packet ic =
  let%lwt c1 = Lwt_io.read_char ic in
  let%lwt c2 = Lwt_io.read_char ic in
  if c1 != dle || c2 != stx
  then wait_for_packet ic
  else Lwt.return ()

let read_escaped_char ic =
  let%lwt c = Lwt_io.read_char ic in
  if c == dle then Lwt_io.read_char ic
  else Lwt.return c

let rec read_packet ic buffer =
  let%lwt c = read_escaped_char ic in
  if c != etx
  then (Buffer.add_char buffer c; read_packet ic buffer)
  else Lwt.return ()

let lwt_main device =
  let f = Unix.openfile device [Unix.O_RDWR] 0644 in
  Logs.info (fun m -> m "Device %s opened" device);
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input f in
  let oc = Unix.out_channel_of_descr f in
  let buffer = Buffer.create 16 in
  output_string oc "\x10\x02Hello, World!\x10\x03"; flush oc;
  let%lwt () = wait_for_packet ic in
  let%lwt () = read_packet ic buffer in
  Logs.info (fun m -> m "Read %s" (Buffer.contents buffer));
  Unix.close f;
  Lwt.return ()

let test_lwt _ device =
  Lwt_main.run (lwt_main device)

let device =
  let doc = "Device" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"DEVICE" ~doc)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let logging =
  let env = Arg.env_var "SERIAL_TEST_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let cmd =
  let doc = "Serial Test" in
  let exits = Term.default_exits in
  Term.(const test_lwt $ logging $ device),
  Term.info "serial_test" ~doc ~exits

let () = Term.(eval cmd |> exit)
