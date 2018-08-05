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
  (* Logs.debug (fun m -> m "%02x" (Char.code c)); *)
  if c == dle then begin
    let%lwt c = Lwt_io.read_char ic in
    (* Logs.debug (fun m -> m "%02x" (Char.code c)); *)
    if c != etx then Lwt.return (Some c)
    else Lwt.return None
  end
  else
    Lwt.return (Some c)

let rec read_packet ic buffer =
  let%lwt c = read_escaped_char ic in
  match c with
  | Some (c) -> (Buffer.add_char buffer c; read_packet ic buffer)
  | None -> Lwt.return ()

let open_device device =
  let f = Unix.openfile device [Unix.O_RDWR] 0644 in
  Logs.info (fun m -> m "Device %s opened" device);
  let state = (ExtUnixSpecific.Ioctl.tiocmget f) land (lnot 0x004) in
  let attr = Unix.tcgetattr f in
  let new_attr = {
    attr with
    c_icrnl = false;
    c_opost = false;
    c_icanon = false;
    c_isig = false;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_ixon = false;
    c_ixoff = false;
    c_ibaud = 115200;
    c_obaud = 115200;
  } in
  let () = Unix.tcsetattr f Unix.TCSANOW new_attr in
  let () = ExtUnixSpecific.Ioctl.tiocmset f state in
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input f in
  let oc = Unix.out_channel_of_descr f in
  (ic, oc)
