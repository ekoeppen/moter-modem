let dle = '\x10'
let stx = '\x02'
let etx = '\x03'

let rec wait_for_packet ic =
  let%lwt c1 = Lwt_io.read_char ic in
  let%lwt c2 = Lwt_io.read_char ic in
  if c1 == dle && c2 == stx
  then Lwt.return ()
  else wait_for_packet ic

let read_escaped_char ic =
  let%lwt c = Lwt_io.read_char ic in
  if c == dle then Lwt_io.read_char ic
  else Lwt.return c

let rec read_packet ic buffer =
  let%lwt c = read_escaped_char ic in
  if c != etx
  then (Buffer.add_char buffer c; read_packet ic buffer)
  else Lwt.return ()

let open_device device =
  let f = Unix.openfile device [Unix.O_RDWR] 0644 in
  Logs.info (fun m -> m "Device %s opened" device);
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input f in
  let oc = Unix.out_channel_of_descr f in
  (ic, oc)

let handle_serial_data device =
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
