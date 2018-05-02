let context = Zmq.Context.create () in
print_endline "Connecting to hello world server...";
let requester = Zmq.Socket.create context Zmq.Socket.req in
Zmq.Socket.connect requester "tcp://localhost:5555";

for i = 1 to 10 do
  Printf.printf "Sending request %d...\n" i;
  Zmq.Socket.send requester "Hello";
  let reply = Zmq.Socket.recv requester in
  Printf.printf "Received reply %d: [%s]\n" i reply
done;

Zmq.Socket.close requester;
Zmq.Context.terminate context
