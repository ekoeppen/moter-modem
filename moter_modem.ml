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

let to_success = function
  | None -> false
  | _ -> true

let handle_heartbeat s =
  let open Core.Option.Let_syntax in
  match%bind Cbor.array_of_string s with
  | (2, s) ->
    let%bind node, s = Cbor.byte_string_of_string s in
    let%map seq, _s = Cbor.int_of_string s in
    Logs.info (fun m -> m "Node: %s seq: %d" node seq)
  | _ -> None

let handle_log_message s =
  let open Core.Option.Let_syntax in
  let%map message, _ = Cbor.byte_string_of_string s in
  Logs.info (fun m -> m "Message: %s" message)

let handle_test_packet s =
  Cbor.int_of_string s

let rec sensor_values_of_string n s values =
  if n > 1 then begin
    let open Core.Option.Let_syntax in
    let%bind t, s = Cbor.tag_of_string s in
    match Cbor.float_of_string s with
    | Some (v, s) ->
        let tag = tag_of_int t in
        Logs.debug (fun m -> m "%s: %f" (string_of_tag tag) v);
        sensor_values_of_string (n - 1) s ((tag, v) :: values)
    | None -> None
  end
  else
    Some (values, s)

let rec report_values node values =
  match values with
  | hd :: tl ->
      Logs.info (fun m -> m "%s/%s: %f" node (fst hd |> string_of_tag) (snd hd));
      report_values node tl
  | [] -> Some (node)

let handle_sensor_reading s =
  let open Core.Option.Let_syntax in
  let%bind n, s = Cbor.array_of_string s in
  let%bind node, s = Cbor.byte_string_of_string s in
  let%map values, _s = sensor_values_of_string n s [] in
  report_values node values

let handle_message s tag client =
  let t = tag_of_int tag in
  Logs.info (fun m -> m "Tag: %s" (string_of_tag t));
  let _ = match t with
    | Sensor_reading_tag -> handle_sensor_reading s |> to_success
    | Test_packet_tag -> handle_test_packet s |> to_success
    | Heartbeat_tag -> handle_heartbeat s |> to_success
    | Log_message_tag -> handle_log_message s |> to_success
    | _ -> Logs.err (fun m -> m "Not a valid start tag %d" tag); false
  in
  Mqtt.pub "test" "123" client

let handle_packet s client =
  match Cbor.tag_of_string s with
  | Some (tag, s) ->
      handle_message s tag client
  | None ->
      (Logs.info (fun m -> m "Message does not start with tag");
      Lwt.return ())

let rec modem_loop ic oc client =
  let packet = Buffer.create 80 in
  let%lwt () = Serial.wait_for_packet ic in
  let%lwt () = Serial.read_packet ic packet in
  let%lwt _ok = handle_packet (Buffer.contents packet) client in
  modem_loop ic oc client

let modem _logging device broker port =
  Logs.debug (fun m -> m "Starting");
  let ic, oc = Serial.open_device device in
  let%lwt client = Mqtt.start_client ~broker ~port ~ca_file:"" ~cert_file:"" ~key_file:"" in
  modem_loop ic oc client

let test_modem _logging _device broker port =
  Logs.info (fun m -> m "Starting using test data");
  let _heartbeat_data = "\x10\x02\xD8\x41\x82\x44Node\x01\x03" in
  let sensor_data = "\x10\x02\xC6\x83\x44Node\xC7\xC4\x82\x20\x0D\xC8\xC4\x82\x20\x18\xFA\x03" in
  let test_data = sensor_data in
  let ic = Lwt_io.of_bytes ~mode:Lwt_io.input (Lwt_bytes.of_string test_data) in
  let oc = Lwt_io.of_bytes ~mode:Lwt_io.output (Lwt_bytes.create 80) in
  let%lwt client = Mqtt.start_client ~broker ~port ~ca_file:"" ~cert_file:"" ~key_file:"" in
  modem_loop ic oc client

let lwt_wrapper logging device broker port =
  Lwt_main.run (test_modem logging device broker port)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let logging_arg =
  let env = Arg.env_var "MOTER_MODEM_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let device_arg =
  let doc = "Device" in
  Arg.(value & opt string "/dev/ttyUSB0" & info ["d"; "device"] ~doc)

let broker_arg =
  let doc = "MQTT broker" in
  Arg.(value & opt string "localhost" & info ["h"; "host"] ~doc)

let port_arg =
  let doc = "Port number" in
  Arg.(value & opt int 1883 & info ["p"; "port"] ~doc)

let cmd =
  let doc = "MoteR Modem" in
  let exits = Term.default_exits in
  Term.(const lwt_wrapper $ logging_arg $ device_arg $ broker_arg $ port_arg),
  Term.info "moter-modem" ~doc ~exits

let () = Term.(eval cmd |> exit)
