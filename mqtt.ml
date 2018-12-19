open Mqtt_lwt

let tls_connect ~broker ~port ~ca_file ~cert_file ~key_file =
  let%lwt authenticator = X509_lwt.authenticator (`Ca_file ca_file) in
  let%lwt certificate = X509_lwt.private_of_pems ~cert:cert_file ~priv_key:key_file in
  let%lwt (ic, oc) = Tls_lwt.connect_ext Tls.Config.(
    client ~authenticator ~certificates:(`Single certificate)
    ~ciphers:Ciphers.supported ()) (broker, port) in
  Lwt.return {ic; oc}

let std_connect ~broker ~port =
  let%lwt socket = Mqtt_lwt.connect ~host:broker ~port:port in
  Logs.debug (fun m -> m "Connected to %s" broker);
  Lwt.return (Mqtt_lwt.of_socket socket)

let start_client ~id ~broker ~port ~ca_file ~cert_file ~key_file =
  Logs.debug (fun m -> m "Connecting to %s" broker);
  let%lwt conn = (if ca_file <> "" && cert_file <> "" && key_file <> "" then
      tls_connect ~broker ~port ~ca_file ~cert_file ~key_file
    else
      std_connect ~broker ~port) in
  Mqtt_lwt.mqtt_client conn ~opts:{default_conn_opts with client_id = id}

let sub topic client =
  Logs.debug (fun m -> m "Subscribing to topic %s" topic);
  Mqtt_lwt.subscribe ~topics:[topic] client.oc

let pub topic message client =
  Logs.debug (fun m -> m "Publishing %s to topic %s" message topic);
  Mqtt_lwt.publish ~topic:topic ~payload:message client.oc

let process client ~f =
  Mqtt_lwt.process_publish_pkt client f
