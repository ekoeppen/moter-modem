let sub topic client =
  Logs.debug (fun m -> m "Subscribing to topic %s" topic);
  Mqtt_lwt.subscribe ~topics:[ topic ] client
;;

let pub topic message client retain =
  Logs.debug (fun m -> m "Publishing %s to topic %s" message topic);
  Mqtt_lwt.publish ~topic ~payload:message ~retain client
;;

let process client ~f = Mqtt_lwt.process_publish_pkt client f
