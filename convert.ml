open Core.Option.Let_syntax

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
  | Modem_message_tag
  | Modem_id_tag
  | RF_packet_tag
  | Mote_info_tag
  | Revision_tag

type response_t = (string option * string) option

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
  | 70 -> Modem_message_tag
  | 71 -> Modem_id_tag
  | 72 -> RF_packet_tag
  | 73 -> Mote_info_tag
  | 74 -> Revision_tag
  | _ -> raise (Failure "Not a known tag")
;;

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
  | Modem_message_tag -> "Modem message"
  | Modem_id_tag -> "Modem ID"
  | RF_packet_tag -> "RF packet"
  | Mote_info_tag -> "Mote info"
  | Revision_tag -> "Revision"
;;

let to_success = function
  | None -> false
  | _ -> true
;;

let handle_heartbeat s : response_t =
  match%bind Cbor.array_of_string s with
  | 2, s ->
    let%bind node, s = Cbor.byte_string_of_string s in
    let%map seq, _s = Cbor.int_of_string s in
    Some node, Printf.sprintf "{\"heartbeat\":%d,\"ts\":%.0f}" seq (Unix.time ())
  | _ -> None
;;

let handle_log_message s =
  let%map message, _ = Cbor.byte_string_of_string s in
  None, Printf.sprintf "{\"log_message\":\"%s\",\"ts\":%.0f}" message (Unix.time ())
;;

let handle_register_value s : response_t =
  match%bind Cbor.array_of_string s with
  | 3, s ->
    let%bind node, s = Cbor.byte_string_of_string s in
    let%bind register, s = Cbor.int_of_string s in
    let%map value, _ = Cbor.int_of_string s in
    ( Some node
    , Printf.sprintf
        "{\"register\":{\"%d\":%d},\"ts\":%.0f}"
        register
        value
        (Unix.time ()) )
  | _ ->
    ();
    let%bind _n, s = Cbor.array_of_string s in
    let%bind register, s = Cbor.int_of_string s in
    let%map value, _ = Cbor.int_of_string s in
    ( None
    , Printf.sprintf
        "{\"register\":{\"%d\":%d},\"ts\":%.0f}"
        register
        value
        (Unix.time ()) )
;;

let handle_error_message s : response_t =
  let%bind _n, s = Cbor.array_of_string s in
  let%bind file, s = Cbor.byte_string_of_string s in
  let%map line, _ = Cbor.int_of_string s in
  None, Printf.sprintf "{\"error\":\"%s:%d\",\"ts\":%.0f}" file line (Unix.time ())
;;

let handle_ping s : response_t =
  let%map node, _s = Cbor.byte_string_of_string s in
  None, Printf.sprintf "{\"ping\":\"%s\",\"ts\":%.0f}" node (Unix.time ())
;;

let handle_test_packet s : response_t =
  let%map v, _s = Cbor.int_of_string s in
  None, Printf.sprintf "{\"test\":\"%d\",\"ts\":%.0f}" v (Unix.time ())
;;

let rec sensor_values_of_string n s values =
  if n > 1
  then (
    let%bind t, s = Cbor.tag_of_string s in
    match Cbor.float_of_string s with
    | Some (v, s) ->
      let tag = tag_of_int t in
      sensor_values_of_string (n - 1) s ((tag, v) :: values)
    | None ->
      (match Cbor.int_of_string s with
      | Some (v, s) ->
        let tag = tag_of_int t in
        sensor_values_of_string (n - 1) s ((tag, float_of_int v) :: values)
      | _ -> None))
  else Some (values, s)
;;

let string_of_values values =
  let s =
    Core.List.fold
      ~init:""
      ~f:(fun acc value ->
        acc ^ Printf.sprintf "\"%s\":%f," (fst value |> string_of_tag) (snd value))
      values
  in
  s ^ Printf.sprintf "\"ts\":%.0f" (Unix.time ())
;;

let handle_sensor_reading s : response_t =
  let%bind n, s = Cbor.array_of_string s in
  let%bind node, s = Cbor.byte_string_of_string s in
  let%map values, _s = sensor_values_of_string n s [] in
  Some node, Printf.sprintf "{%s}" (string_of_values values)
;;

let handle_message s tag : response_t =
  match tag with
  | Log_message_tag -> handle_log_message s
  | Heartbeat_tag -> handle_heartbeat s
  | Test_packet_tag -> handle_test_packet s
  | Ping_tag -> handle_ping s
  | Sensor_reading_tag -> handle_sensor_reading s
  | Register_value_tag -> handle_register_value s
  | Error_message_tag -> handle_error_message s
  | _ -> None
;;

let handle_swap_packet s : response_t =
  let src = String.get s 1 |> Char.code in
  let func = String.get s 4 |> Char.code in
  let reg = String.get s 6 |> Char.code in
  let reglen = String.length s - 7 in
  let regstr = Core.String.suffix s reglen in
  let r1 =
    ((String.get regstr 0 |> Char.code) * 256) + (String.get regstr 1 |> Char.code)
    |> float_of_int
  in
  let r2 =
    (if reglen > 2
    then ((String.get regstr 2 |> Char.code) * 256) + (String.get regstr 3 |> Char.code)
    else 0)
    |> float_of_int
  in
  let time = Unix.time () in
  match func with
  | 0 ->
    (match reg with
    | 11 ->
      Some
        ( Some (Printf.sprintf "%08X" src)
        , Printf.sprintf "{\"Voltage\":%.3f, \"ts\":%.0f}" (r1 /. 1000.0) time )
    | 12 ->
      Some
        ( Some (Printf.sprintf "%08X" src)
        , if reglen == 2
          then
            Printf.sprintf
              "{\"Temperature\":%.3f, \"ts\":%.0f}"
              ((r1 /. 10.0) -. 50.0)
              time
          else
            Printf.sprintf
              "{\"Temperature\":%.1f, \"Humidity\":%.1f, \"ts\":%.0f}"
              ((r1 /. 10.0) -. 50.0)
              (r2 /. 10.)
              time )
    | _ -> None)
  | _ -> None
;;

let handle_rf_packet s : response_t =
  let%bind packet, _s = Cbor.byte_string_of_string s in
  let%map response =
    match Cbor.tag_of_string packet with
    | Some (packet_tag, p) -> handle_message p (tag_of_int packet_tag)
    | None -> handle_swap_packet packet
  in
  response
;;

let handle_modem_message msg =
  let%bind _n, s = Cbor.array_of_string msg in
  let%bind _t1, s = Cbor.tag_of_string s in
  let%bind modem, s = Cbor.int_of_string s in
  let%bind msg_tag, s = Cbor.tag_of_string s in
  let%map response =
    match tag_of_int msg_tag with
    | RF_packet_tag -> handle_rf_packet s
    | t -> handle_message s t
  in
  modem, fst response, snd response
;;

let handle_packet p =
  let s = Hex.to_string (`Hex p) in
  let%bind packet_tag, s = Cbor.tag_of_string s in
  match tag_of_int packet_tag with
  | Modem_message_tag -> handle_modem_message s
  | _ -> None
;;
