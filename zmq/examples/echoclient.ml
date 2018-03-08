(* Simple client *)

let ctx = ZMQ.Context.create () in
let socket = ZMQ.Socket.create ctx ZMQ.Socket.dealer in
ZMQ.Socket.connect socket "tcp://127.0.0.1:5555";

let msg =
  let open Bigarray in
  let data = Array1.create char c_layout 1_000_000 in
  Array1.fill data '\x00';
  ZMQ.Msg.init_data data
in

while true do
  ZMQ.Socket.send_msg socket msg;
  ignore (ZMQ.Socket.recv_msg socket);
done;

ZMQ.Socket.close socket;
ZMQ.Context.terminate ctx
