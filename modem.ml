open Cmdliner

type tag_t =
  | Sensor_reading_tag
  | Voltage_tag
  | Temperature_tag
  | Humidity_tag
  | Pressure_tag
  | Lux_tag
  | Uv_index_tag
  | Motion_tag
  | Sound_level_tag
  | Co2_tag
  | Test_packet_tag
  | Heartbeat_tag
  | Log_message_tag
  | Ping_tag
  | Register_value_tag
  | Error_message_tag

type response_t = (string * string) list option

let tag_of_int t =
  match t with
  | 6 -> Sensor_reading_tag
  | 7 -> Voltage_tag
  | 8 -> Temperature_tag
  | 9 -> Humidity_tag
  | 10 -> Pressure_tag
  | 11 -> Lux_tag
  | 12 -> Uv_index_tag
  | 13 -> Motion_tag
  | 14 -> Sound_level_tag
  | 15 -> Co2_tag
  | 64 -> Test_packet_tag
  | 65 -> Heartbeat_tag
  | 66 -> Log_message_tag
  | 67 -> Ping_tag
  | 68 -> Register_value_tag
  | 69 -> Error_message_tag
  | _ -> raise (Failure "Not a known tag")

let string_of_tag t =
  match t with
  | Sensor_reading_tag -> "Sensor reading"
  | Voltage_tag -> "Voltage"
  | Temperature_tag -> "Temperature"
  | Humidity_tag -> "Humidity"
  | Pressure_tag -> "Pressure"
  | Lux_tag -> "Lux"
  | Uv_index_tag -> "UV index"
  | Motion_tag -> "Motion"
  | Sound_level_tag -> "Sound level"
  | Co2_tag -> "CO2"
  | Test_packet_tag -> "Test packet"
  | Heartbeat_tag -> "Heartbeat"
  | Log_message_tag -> "Log message"
  | Ping_tag -> "Ping"
  | Register_value_tag -> "Register value"
  | Error_message_tag -> "Error message"

let to_success = function
  | None -> false
  | _ -> true

type channels_t = {
  ic : Lwt_io.input Lwt_io.channel;
  oc : out_channel;
  client : Mqtt_lwt.t;
  prefix : string;
}

let handle_heartbeat s =
  let open Core.Option.Let_syntax in
  match%bind Cbor.array_of_string s with
  | (2, s) ->
    let%bind node, s = Cbor.byte_string_of_string s in
    let%map seq, _s = Cbor.int_of_string s in
    Logs.info (fun m -> m "Heartbeat: %s %d" node seq);
    [
      (
        "/" ^ node,
        (Printf.sprintf "{\"heartbeat\":%d,\"ts\":%.0f}" seq (Unix.time ()))
      )
    ]
  | _ -> None

let handle_log_message s : response_t =
  let open Core.Option.Let_syntax in
  let%map message, _ = Cbor.byte_string_of_string s in
  Logs.info (fun m -> m "Log: %s" message);
  [
    (
      "/Modem",
      (Printf.sprintf "{\"log_message\":\"%s\",\"ts\":%.0f}" message (Unix.time ()))
    )
  ]

let handle_register_value s : response_t =
  let open Core.Option.Let_syntax in
  let%bind _n, s = Cbor.array_of_string s in
  let%bind register, s = Cbor.int_of_string s in
  let%map value, _ = Cbor.int_of_string s in
  Logs.info (fun m -> m "Register: %02x %02x" register value);
  [
    (
      "/Modem",
      (Printf.sprintf "{\"register\":{\"%d\":%d},\"ts\":%.0f}" register value (Unix.time ()))
    )
  ]

let handle_error_message s : response_t =
  let open Core.Option.Let_syntax in
  let%bind _n, s = Cbor.array_of_string s in
  let%bind file, s = Cbor.byte_string_of_string s in
  let%map line, _ = Cbor.int_of_string s in
  Logs.err (fun m -> m "Program error: %s %d" file line);
  [
    (
      "/Modem",
      (Printf.sprintf "{\"error\":\"%s:%d\",\"ts\":%.0f}" file line (Unix.time ()))
    )
  ]

let handle_ping s =
  let open Core.Option.Let_syntax in
  let%map node, _s = Cbor.byte_string_of_string s in
  Logs.info (fun m -> m "Ping: %s" node);
  [
    (
      "/Modem",
      (Printf.sprintf "{\"ping\":\"%s\",\"ts\":%.0f}" node (Unix.time ()))
    )
  ]

let handle_test_packet s =
  let open Core.Option.Let_syntax in
  let%map v, _s = Cbor.int_of_string s in
  [
    (
      "/Modem",
      (Printf.sprintf "{\"test\":\"%d\",\"ts\":%.0f}" v (Unix.time ()))
    )
  ]

let rec sensor_values_of_string n s values =
  if n > 1 then begin
    let open Core.Option.Let_syntax in
    let%bind t, s = Cbor.tag_of_string s in
    match Cbor.float_of_string s with
    | Some (v, s) ->
        let tag = tag_of_int t in
        Logs.debug (fun m -> m "%s: %f" (string_of_tag tag) v);
        sensor_values_of_string (n - 1) s ((tag, v) :: values)
    | _ -> None
  end
  else
    Some (values, s)

let string_of_values values =
  let s = Core.List.fold
    ~init:""
    ~f:(fun acc value ->
      acc ^ (Printf.sprintf "\"%s\":%f," (fst value |> string_of_tag) (snd value)))
    values in
  s ^ (Printf.sprintf "\"ts\":%.0f" (Unix.time ()))

let rec publish_messages m client prefix =
  match m with
  | hd :: tl ->
      let%lwt () = Mqtt.pub (prefix ^ (fst hd)) (snd hd) client in
      publish_messages tl client prefix
  | [] -> Lwt.return ()

let handle_sensor_reading s =
  let open Core.Option.Let_syntax in
  Logs.info (fun m -> m "Sensor data");
  let%bind n, s = Cbor.array_of_string s in
  let%bind node, s = Cbor.byte_string_of_string s in
  let%map values, _s = sensor_values_of_string n s [] in
  [
    (
      "/" ^ node,
      Printf.sprintf "{%s}" (string_of_values values)
    )
  ]

let handle_message s tag =
  let t = tag_of_int tag in
  Logs.debug (fun m -> m "Tag: %s" (string_of_tag t));
  match t with
    | Log_message_tag -> handle_log_message s
    | Heartbeat_tag -> handle_heartbeat s
    | Test_packet_tag -> handle_test_packet s
    | Ping_tag -> handle_ping s
    | Sensor_reading_tag -> handle_sensor_reading s
    | Register_value_tag -> handle_register_value s
    | Error_message_tag -> handle_error_message s
    | _ -> Logs.err (fun m -> m "Not a valid start tag %d" tag); None

let handle_packet s client prefix =
  match Cbor.tag_of_string s with
  | Some (tag, s) ->
      (match handle_message s tag with
      | Some response -> publish_messages response client prefix
      | None -> Lwt.return ())
  | None ->
      (Logs.info (fun m -> m "Message does not start with tag");
      Lwt.return ())

let rec modem_loop channels =
  let packet = Buffer.create 80 in
  let%lwt () = Serial.wait_for_packet channels.ic in
  let%lwt () = Serial.read_packet channels.ic packet in
  let%lwt _ok = handle_packet (Buffer.contents packet)
    channels.client channels.prefix in
  modem_loop channels

let display_topic channels _ topic payload id =
  Logs.info (fun m -> m "Topic: %s Payload: %s Msg_id: %d" topic payload id);
  let cmd = `Hex payload in
  let bin_cmd = Hex.to_string cmd in
  Serial.write_packet channels.oc bin_cmd;
  Lwt.return ()

let rec command_loop channels =
  let%lwt () = Mqtt.process channels.client ~f:(display_topic channels) in
  command_loop channels

let modem _logging ~device ~broker ~port ~proxy ~prefix ~certs =
  Logs.debug (fun m -> m "Start reporting to %s:%d/%s" broker port prefix);
  if (Mqtt.valid certs) then Logs.debug (fun m -> m "Certs: %s %s %s" certs.cert certs.key certs.ca);
  if (proxy <> "") then Logs.debug (fun m -> m "Proxy: %s" proxy);
  let ic, oc = Serial.open_device device in
  let id = Random.self_init (); Random.bits () |>
    Printf.sprintf "mqtt_lwt_%d" in
  let%lwt client = Mqtt.start_client ~id ~broker ~port ~proxy ~certs in
  let%lwt () = Mqtt.sub (prefix ^ "/Modem/Cmd") client in
  let channels = {ic = ic; oc = oc; prefix = prefix; client = client} in
  Lwt.pick [modem_loop channels; command_loop channels]

let lwt_wrapper logging device broker port proxy ca_file cert_file key_file prefix =
  let certs : Mqtt.certs_t = {
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
  let env = Arg.env_var "MOTER_MODEM_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let proxy_arg =
  let doc = "SOCKS5 proxy" in
  let env = Arg.env_var "MOTER_SOCKS_PROXY" in
  Arg.(value & opt string "" & info ["proxy"] ~env ~doc)

let device_arg =
  let doc = "Device" in
  let env = Arg.env_var "MOTER_DEVICE" in
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
  let exits = Term.default_exits in
  Term.(const lwt_wrapper $ logging_arg $ device_arg $
    broker_arg $ port_arg $ proxy_arg $
    ca_file $ cert_file $ key_file $
    prefix_arg),
  Term.info "moter-modem" ~doc ~exits

let () = Term.(eval cmd |> exit)
