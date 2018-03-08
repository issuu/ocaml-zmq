(* Simple echo server with no copying of the received message
   content *)

let ctx = ZMQ.Context.create () in
let socket = ZMQ.Socket.create ctx ZMQ.Socket.router in
ZMQ.Socket.bind socket "tcp://*:5555";

let counter = ref 0 in

while true do
  incr counter;
  Printf.printf "Round %d\n%!" !counter;
  let sender = ZMQ.Socket.recv_msg socket in
  let msg = ZMQ.Socket.recv_msg socket in
  ZMQ.Socket.send_msg ~more:true socket sender;
  ZMQ.Socket.send_msg socket msg;
done;

ZMQ.Socket.close socket;
ZMQ.Context.terminate ctx
