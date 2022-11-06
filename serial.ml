let reset f =
  ExtUnix.Specific.Ioctl.tiocmset f 2;
  Unix.sleepf 0.1;
  ExtUnix.Specific.Ioctl.tiocmset f 6;
  Unix.sleepf 0.1;
  ExtUnix.Specific.Ioctl.tiocmset f 2
;;

let open_device device =
  let f = Unix.openfile device [ Unix.O_RDWR ] 0644 in
  let attr = Unix.tcgetattr f in
  let new_attr =
    { attr with
      c_icrnl = false
    ; c_opost = false
    ; c_icanon = false
    ; c_isig = false
    ; c_echo = false
    ; c_echoe = false
    ; c_echok = false
    ; c_ixon = false
    ; c_ixoff = false
    ; c_ibaud = 115200
    ; c_obaud = 115200
    }
  in
  Unix.tcsetattr f Unix.TCSANOW new_attr;
  reset f;
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input f in
  let oc = Lwt_io.of_unix_fd ~mode:Lwt_io.output f in
  ic, oc
;;
