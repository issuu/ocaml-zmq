
let context = ZMQ.init 1 in

let frontend = ZMQ.socket context ZMQ.Sub in
ZMQ.connect frontend "tcp://192.168.55.210:5556";
print_endline "Socket connected";
let backend = ZMQ.socket context ZMQ.Pub in
ZMQ.bind backend "tcp://10.1.1.0:8100";
print_endline "Socket bound";
ZMQ.setsockopt frontend (`Subscribe(""));
while true do
  let finish = ref false in
  while not !finish do
    let message = ZMQ.recv frontend ZMQ.None in
    print_endline message;
    match ZMQ.getsockoption frontend `Recieve_more with
      | `Recieve_more(1L) ->
        ZMQ.send backend message ZMQ.Snd_more
      | _ ->
        begin
          ZMQ.send backend message ZMQ.None;
          finish := true
        end
  done
done;

ZMQ.close frontend;
ZMQ.close backend;
ZMQ.term context

