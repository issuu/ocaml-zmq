module ZSocket = ZMQ.Socket;;

let context = ZMQ.init () in

let frontend = ZSocket.create context ZSocket.sub in
ZSocket.connect frontend "tcp://192.168.55.210:5556";

let backend = ZSocket.create context ZSocket.sub in
ZSocket.bind backend "tcp://10.1.1.0:8100";

ZSocket.subscribe frontend "";

while true do
  let finish = ref false in
  while not !finish do
    let message = ZSocket.recv frontend in
    if ZSocket.has_more frontend then
      ZSocket.send ~more:true backend message
    else begin
      ZSocket.send backend message;
      finish := true
    end
  done
done;

ZSocket.close frontend;
ZSocket.close backend;
ZMQ.term context

