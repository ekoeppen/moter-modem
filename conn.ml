type certs_t =
  { key : string
  ; cert : string
  ; ca : string
  }

let valid certs = certs.ca <> "" && certs.cert <> "" && certs.key <> ""

let connect_socket host port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let%lwt host_info = Lwt_unix.gethostbyname host in
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  let%lwt () = Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port)) in
  Lwt.return socket
;;

let of_socket socket authenticator certificate =
  Tls_lwt.Unix.client_of_fd
    Tls.Config.(
      client
        ~authenticator
        ~certificates:(`Single certificate)
        ~ciphers:Ciphers.supported
        ())
    socket
;;

let tls_connect ~host ~port ~certs =
  let%lwt authenticator = X509_lwt.authenticator (`Ca_file certs.ca) in
  let%lwt certificate = X509_lwt.private_of_pems ~cert:certs.cert ~priv_key:certs.key in
  let%lwt ic, oc =
    Tls_lwt.connect_ext
      Tls.Config.(
        client
          ~authenticator
          ~certificates:(`Single certificate)
          ~ciphers:Ciphers.supported
          ())
      (host, port)
  in
  Lwt.return (ic, oc)
;;

let std_connect ~host ~port =
  let%lwt socket = connect_socket host port in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  Lwt.return (ic, oc)
;;

let connect ~host ~port ~certs =
  Logs.debug (fun m -> m "Connecting to %s" host);
  let%lwt conn =
    if valid certs then tls_connect ~host ~port ~certs else std_connect ~host ~port
  in
  Logs.debug (fun m -> m "Connected to %s" host);
  Lwt.return conn
;;
