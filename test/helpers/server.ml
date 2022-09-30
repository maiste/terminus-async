module J = Ezjsonm

let extract_body body =
  let open Httpaf in
  let buffer = Buffer.create 1024 in
  let on_read str ~off:_ ~len:_ =
    Bigstringaf.to_string str |> Buffer.add_string buffer
  in
  let on_eof () = () in
  Body.schedule_read body ~on_eof ~on_read;
  Buffer.contents buffer

(* Streaming write to handle EOF. *)
let write_body_and_close reqd body =
  let open Httpaf in
  let resp =
    Response.create
      ~headers:
        (Headers.of_list
           [ ("content-type", "application/json"); ("connection", "close") ])
      `OK
  in
  let response_body = Reqd.respond_with_streaming reqd resp in
  Body.write_string response_body body;
  Body.close_writer response_body

let check_headers headers =
  assert (Httpaf.Headers.get headers "X-Auth-Token" = Some "mytoken");
  assert (Httpaf.Headers.get headers "Content-Type" = Some "application/json");
  ()

(* Many assert false as this is not supposed to test the web server but
   the API *)
let request_handler _ reqd =
  let open Httpaf in
  let request = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  let body = extract_body request_body in
  check_headers request.Request.headers;
  match request with
  | { Request.meth = `GET; _ } ->
      let body = `O [ ("id", `Float 1.0) ] |> J.value_to_string in
      write_body_and_close reqd body
  | { Request.meth = `POST | `PUT; _ } ->
      let json = J.value_from_string body in
      let userId = J.find json [ "userId" ] |> J.get_float in
      let body = `O [ ("userId", `Float userId) ] |> J.value_to_string in
      write_body_and_close reqd body
  | { Request.meth = `DELETE; _ } -> write_body_and_close reqd "{}"
  | _ -> assert false

let error_handler _ ?request:_ error start_response =
  let open Httpaf in
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn);
      Body.write_string response_body "\n"
  | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body

open Async.Deferred.Infix

let with_server ~port f x =
  let where_to_listen = Async.Tcp.Where_to_listen.of_port port in
  Async.Tcp.Server.create_sock ~on_handler_error:`Raise where_to_listen
    (Httpaf_async.Server.create_connection_handler ~request_handler
       ~error_handler)
  >>= fun server ->
  try
    let res = f x in
    Async.Tcp.Server.close server |> Async.don't_wait_for;
    res
  with exn ->
    Async.Tcp.Server.close server |> Async.Deferred.don't_wait_for;
    raise exn
