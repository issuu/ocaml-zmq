let context = ZMQ.Context.create () in
print_endline "Connecting to hello world server...";
let requester = ZMQ.Socket.create context ZMQ.Socket.req in
ZMQ.Socket.connect requester "tcp://localhost:5555";

for i = 1 to 10 do
  Printf.printf "Sending request %d...\n" i;
  ZMQ.Socket.send requester "Hello";
  let reply = ZMQ.Socket.recv requester in
  Printf.printf "Received reply %d: [%s]\n" i reply
done;

ZMQ.Socket.close requester;
ZMQ.Context.terminate context
