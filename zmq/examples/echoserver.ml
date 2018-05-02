(* Simple echo server with no copying of the received message
   content *)

let ctx = Zmq.Context.create () in
let socket = Zmq.Socket.create ctx Zmq.Socket.router in
Zmq.Socket.bind socket "tcp://*:5555";

let counter = ref 0 in

while true do
  incr counter;
  Printf.printf "Round %d\n%!" !counter;
  let sender = Zmq.Socket.recv_msg socket in
  let msg = Zmq.Socket.recv_msg socket in
  Zmq.Socket.send_msg ~more:true socket sender;
  Zmq.Socket.send_msg socket msg;
done;

Zmq.Socket.close socket;
Zmq.Context.terminate ctx
