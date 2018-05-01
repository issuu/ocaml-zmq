let context = Zmq.Context.create () in
let responder = Zmq.Socket.create context Zmq.Socket.rep in
Zmq.Socket.bind responder "tcp://*:5555";

while true do
  let request = Zmq.Socket.recv responder in
  Printf.printf "Received request: [%s]\n%!" request;
  Zmq.Socket.send responder "World"
done;

Zmq.Socket.close responder;
Zmq.Context.terminate context
