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
  let state = (ExtUnixSpecific.Ioctl.tiocmget f) land (lnot 0x004) in
  let () = ExtUnixSpecific.Ioctl.tiocmset f state in
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input f in
  let oc = Unix.out_channel_of_descr f in
  (ic, oc)
