open ZMQ;;
open ZMQ.Socket;;

let context = init () in
print_endline "Connecting to hello world server...";
let requester = Socket.create context req in
connect requester "tcp://localhost:5555";

for i = 1 to 10 do
  Printf.printf "Sending request %d...\n" i;
  send requester "Hello";
  let reply = recv requester in
  Printf.printf "Received reply %d: [%s]\n" i reply
done;

close requester;
term context
