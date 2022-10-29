open Cmdliner

type channels_t = {
  ic : Lwt_io.input Lwt_io.channel;
  oc : out_channel;
  client : Mqtt_lwt.t;
  prefix : string;
}

let publish_message m client prefix =
  match m with
  | (_, Some node, regs) ->
      Logs.debug (fun m -> m "%s/Node/%s %s\n" prefix node regs);
      Mqtt.pub (prefix ^ "/Node/" ^ node) regs client true
  | (modem, None, regs) ->
      Logs.debug (fun m -> m "%s/Modem/%X %s\n" prefix modem regs);
      Mqtt.pub (Printf.sprintf "%s/Modem/%X" prefix modem) regs client true

let handle_packet s client prefix =
  match Convert.handle_packet s with
  | Some response -> publish_message response client prefix
  | None -> Lwt.return ()

let rec modem_loop channels =
  let%lwt packet = Lwt_io.read_line channels.ic in
  Logs.debug (fun m -> m "Packet: %s" packet);
  let%lwt () = handle_packet packet channels.client channels.prefix in
  modem_loop channels

let modem _logging ~device ~broker ~port ~proxy ~prefix ~certs =
  Logs.debug (fun m -> m "Start reporting to %s:%d/%s" broker port prefix);
  if (Conn.valid certs) then Logs.debug (fun m -> m "Certs: %s %s %s" certs.cert certs.key certs.ca);
  if (proxy <> "") then Logs.debug (fun m -> m "Proxy: %s" proxy);
  let ic, oc = Serial.open_device device in
  let id = Random.self_init (); Random.bits () |>
    Printf.sprintf "mqtt_lwt_%d" in
  let%lwt (mqtt_ic, mqtt_oc) = Conn.connect ~host:broker ~port ~proxy ~certs in
  let%lwt client = Mqtt_lwt.connect {oc = mqtt_oc; ic = mqtt_ic} ~opts:{Mqtt_lwt.default_conn_opts with client_id = id} in
  let%lwt () = Mqtt_lwt.subscribe ~topics:[prefix ^ "/Modem/Cmd"] client in
  let channels = {ic = ic; oc = oc; prefix = prefix; client = client} in
  Lwt.pick [
    Mqtt_lwt.run client;
    modem_loop channels;
  ]

let lwt_wrapper logging device broker port proxy ca_file cert_file key_file prefix =
  let certs : Conn.certs_t = {
    cert = cert_file;
    key = key_file;
    ca = ca_file} in
  Lwt_main.run (modem logging ~device ~broker ~port ~proxy ~prefix ~certs)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let logging_arg =
  let env = Cmd.Env.info "MOTER_MODEM_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let proxy_arg =
  let doc = "SOCKS5 proxy" in
  let env = Cmd.Env.info "MOTER_SOCKS_PROXY" in
  Arg.(value & opt string "" & info ["proxy"] ~env ~doc)

let device_arg =
  let doc = "Device" in
  let env = Cmd.Env.info "MOTER_DEVICE" in
  Arg.(value & opt string "/dev/ttyUSB0" & info ["d"; "device"] ~env ~doc)

let broker_arg =
  let doc = "MQTT broker" in
  Arg.(value & opt string "localhost" & info ["h"; "host"] ~doc)

let port_arg =
  let doc = "Port number" in
  Arg.(value & opt int 1883 & info ["p"; "port"] ~doc)

let prefix_arg =
  let doc = "MQTT prefix" in
  Arg.(value & opt string "" & info ["prefix"] ~doc)

let ca_file =
  let doc = "CA file" in
  Arg.(value & opt non_dir_file "" & info ["ca"] ~doc)

let cert_file =
  let doc = "Client certificate file" in
  Arg.(value & opt non_dir_file "" & info ["cert"] ~doc)

let key_file =
  let doc = "Client key" in
  Arg.(value & opt non_dir_file "" & info ["key"] ~doc)

let cmd =
  let doc = "MoteR Modem" in
  let term = Term.(const lwt_wrapper $ logging_arg $ device_arg $
    broker_arg $ port_arg $ proxy_arg $
    ca_file $ cert_file $ key_file $
    prefix_arg) in
  let info = Cmd.info doc in
  Cmd.v info term

let () = Cmd.eval cmd |> exit
