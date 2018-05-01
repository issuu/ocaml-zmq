(* Simple client *)

let ctx = Zmq.Context.create () in
let socket = Zmq.Socket.create ctx Zmq.Socket.dealer in
Zmq.Socket.connect socket "tcp://127.0.0.1:5555";

let msg =
  let open Bigarray in
  let data = Array1.create char c_layout 1_000_000 in
  Array1.fill data '\x00';
  Zmq.Msg.init_data data
in

while true do
  Zmq.Socket.send_msg socket msg;
  ignore (Zmq.Socket.recv_msg socket);
done;

Zmq.Socket.close socket;
Zmq.Context.terminate ctx
