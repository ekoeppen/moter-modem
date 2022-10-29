open Rresult

type certs_t =
  { key : string
  ; cert : string
  ; ca : string
  }

let valid certs = certs.ca <> "" && certs.cert <> "" && certs.key <> ""

let get_proxy_setting proxy =
  let uri = Uri.of_string proxy in
  match Uri.host uri with
  | Some host ->
    (match Uri.port uri with
    | Some port -> host, port
    | None -> host, 1080)
  | None -> "", 0
;;

let connect_socket host port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let%lwt host_info = Lwt_unix.gethostbyname host in
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  let%lwt () = Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port)) in
  Logs.debug (fun m -> m "Socket connection to host %s" host);
  Lwt.return socket
;;

let send_connect_request socket host port =
  let connect_str =
    R.get_ok
      (Socks_client.make_socks5_request (Connect { address = Domain_address host; port }))
  in
  Lwt_unix.write_string socket connect_str 0 (String.length connect_str)
;;

let proxy_connect_socket proxy_host proxy_port host port =
  let%lwt socket = connect_socket proxy_host proxy_port in
  let auth_request = Socks_client.make_socks5_auth_request ~username_password:false in
  let auth_response = Bytes.make 2 '\x00' in
  let connect_response = Bytes.make 10 '\x00' in
  let%lwt _ = Lwt_unix.write_string socket auth_request 0 (String.length auth_request) in
  let%lwt _ = Lwt_unix.read socket auth_response 0 2 in
  let auth_method =
    Socks_client.parse_socks5_auth_response (Bytes.to_string auth_response)
  in
  (match auth_method with
  | No_acceptable_methods -> Logs.err (fun m -> m "No acceptable auth methods")
  | _ -> Logs.debug (fun m -> m "Auth OK"));
  let%lwt _ = send_connect_request socket host port in
  let%lwt _ = Lwt_unix.read socket connect_response 0 10 in
  let c = Socks_client.parse_socks5_response (Bytes.to_string connect_response) in
  (match c with
  | Ok _ -> Logs.debug (fun m -> m "Connect request ok")
  | Error _ -> Logs.err (fun m -> m "Connect failed"));
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

let tls_connect ~host ~port ~proxy ~certs =
  let%lwt authenticator = X509_lwt.authenticator (`Ca_file certs.ca) in
  let%lwt certificate = X509_lwt.private_of_pems ~cert:certs.cert ~priv_key:certs.key in
  let proxy_host, proxy_port = get_proxy_setting proxy in
  if proxy_host <> ""
  then (
    let () = Logs.info (fun m -> m "Connecting via proxy %s:%d" proxy_host proxy_port) in
    let%lwt socket = proxy_connect_socket proxy_host proxy_port host port in
    let%lwt tls = of_socket socket authenticator certificate in
    Logs.debug (fun m -> m "TLS handshake complete");
    let ic, oc = Tls_lwt.of_t tls in
    Lwt.return (ic, oc))
  else (
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
    Lwt.return (ic, oc))
;;

let std_connect ~host ~port ~proxy =
  let proxy_host, proxy_port = get_proxy_setting proxy in
  let%lwt socket =
    if proxy_host <> ""
    then (
      let () =
        Logs.info (fun m -> m "Connecting via proxy %s:%d" proxy_host proxy_port)
      in
      proxy_connect_socket proxy_host proxy_port host port)
    else connect_socket host port
  in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  Lwt.return (ic, oc)
;;

let connect ~host ~port ~proxy ~certs =
  Logs.debug (fun m -> m "Connecting to %s" host);
  let%lwt conn =
    if valid certs
    then tls_connect ~host ~port ~proxy ~certs
    else std_connect ~host ~port ~proxy
  in
  Logs.debug (fun m -> m "Connected to %s" host);
  Lwt.return conn
;;
