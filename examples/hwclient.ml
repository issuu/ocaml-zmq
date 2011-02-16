open ZMQ;;

let context = init 1 in
print_endline "Connecting to hello world server...";
let requester = socket context Req in
connect requester "tcp://localhost:5555";

for i = 1 to 10 do
  Printf.printf "Sending request %d...\n" i;
  send requester "Hello" None;
  let reply = recv requester None in
  Printf.printf "Received reply %d: [%s]\n" i reply
done;

close requester;
term context

