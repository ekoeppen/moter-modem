open Cmdliner

type channels_t =
  { ic : Lwt_io.input Lwt_io.channel
  ; oc : Lwt_io.output Lwt_io.channel
  }

type t =
  { serial : channels_t
  ; client : Mqtt_lwt.t
  ; prefix : string
  ; id : string
  }

let fail_connect_on id e =
  Logs.err (fun m -> m "[%s] Exception during connect: %s" id (Printexc.to_string e));
  match e with
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
    Logs.info (fun m -> m "MQTT host refused connection, retrying");
    Unix.sleep (1 * 60);
    false
  | Not_found ->
    Logs.info (fun m -> m "MQTT host not found, retrying");
    Unix.sleep (15 * 60);
    false
  | Lwt_unix.Timeout ->
    Logs.err (fun m -> m "Protocol error during connection, stopping");
    true
  | _ -> true
;;

let fail_run_on modem e =
  Logs.err (fun m -> m "[%s] Exception during run: %s" modem.id (Printexc.to_string e));
  match e with
  | Lwt_unix.Timeout ->
    Logs.info (fun m -> m "Timeout waiting for modem message, restarting");
    Unix.sleep 30;
    false
  | Lwt_stream.Empty ->
    Logs.info (fun m -> m "Lost connection to MQTT host, restarting");
    Unix.sleep (1 * 60);
    false
  | _ -> true
;;

let close t =
  let%lwt () = Lwt_io.close t.serial.oc in
  Lwt_io.close t.client.oc
;;

let rec connect ~host ~port ~certs ~id =
  try%lwt
    let%lwt mqtt_ic, mqtt_oc = Conn.connect ~host ~port ~certs in
    Mqtt_lwt.connect
      { oc = mqtt_oc; ic = mqtt_ic }
      ~opts:{ Mqtt_lwt.default_conn_opts with client_id = id }
  with
  | e -> if fail_connect_on id e then exit 1 else connect ~host ~port ~certs ~id
;;

let publish_message m client prefix =
  match m with
  | _, Some node, regs ->
    Logs.debug (fun m -> m "%s/Node/%s %s\n" prefix node regs);
    Mqtt.pub (prefix ^ "/Node/" ^ node) regs client true
  | modem, None, regs ->
    Logs.debug (fun m -> m "%s/Modem/%X %s\n" prefix modem regs);
    Mqtt.pub (Printf.sprintf "%s/Modem/%X" prefix modem) regs client true
;;

let handle_packet s client prefix =
  match Convert.handle_packet s with
  | Some response -> publish_message response client prefix
  | None -> Lwt.return ()
;;

let rec modem_loop modem =
  let%lwt packet =
    Lwt_unix.with_timeout 60.0 (fun () -> Lwt_io.read_line modem.serial.ic)
  in
  Logs.debug (fun m -> m "Packet: %s" packet);
  let%lwt () = handle_packet packet modem.client modem.prefix in
  modem_loop modem
;;

let rec run ~device ~host ~port ~prefix ~certs =
  let id = Random.bits () |> Printf.sprintf "mm_%d" in
  let serial_ic, serial_oc = Serial.open_device device in
  let serial = { ic = serial_ic; oc = serial_oc } in
  let%lwt client = connect ~host ~port ~certs ~id in
  let modem = { serial; client; id; prefix } in
  Logs.info (fun m ->
      m "[%s] Start reporting from %s to %s:%d/%s" id device host port prefix);
  try%lwt Lwt.pick [ Mqtt_lwt.run modem.client; modem_loop modem ] with
  | e ->
    if fail_run_on modem e
    then Lwt.return ()
    else (
      let%lwt () = close modem in
      run ~device ~host ~port ~prefix ~certs)
;;

let lwt_wrapper _logging device host port ca_file cert_file key_file prefix =
  Random.self_init ();
  let certs : Conn.certs_t = { cert = cert_file; key = key_file; ca = ca_file } in
  Lwt_main.run (run ~device ~host ~port ~prefix ~certs)
;;

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
;;

let logging_arg = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let device_arg =
  let doc = "Device" in
  Arg.(value & opt string "/dev/ttyUSB0" & info [ "d"; "device" ] ~doc)
;;

let host_arg =
  let doc = "MQTT host" in
  Arg.(value & opt string "localhost" & info [ "h"; "host" ] ~doc)
;;

let port_arg =
  let doc = "Port number" in
  Arg.(value & opt int 1883 & info [ "p"; "port" ] ~doc)
;;

let prefix_arg =
  let doc = "MQTT prefix" in
  Arg.(value & opt string "" & info [ "prefix" ] ~doc)
;;

let ca_file =
  let doc = "CA file" in
  Arg.(value & opt non_dir_file "" & info [ "ca" ] ~doc)
;;

let cert_file =
  let doc = "Client certificate file" in
  Arg.(value & opt non_dir_file "" & info [ "cert" ] ~doc)
;;

let key_file =
  let doc = "Client key" in
  Arg.(value & opt non_dir_file "" & info [ "key" ] ~doc)
;;

let cmd =
  let doc = "MoteR Modem" in
  let term =
    Term.(
      const lwt_wrapper
      $ logging_arg
      $ device_arg
      $ host_arg
      $ port_arg
      $ ca_file
      $ cert_file
      $ key_file
      $ prefix_arg)
  in
  let info = Cmd.info doc in
  Cmd.v info term
;;

let () = Cmd.eval cmd |> exit
