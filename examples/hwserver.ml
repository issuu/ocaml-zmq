open ZMQ;;

let context = init 1 in
let responder = socket context Rep in
bind responder "tcp://*:5555";
while true do
  let request = recv responder None in
  Printf.printf "Received request: [%s]\n%!" request;
  send responder "World" None
done;

close responder;
term context
