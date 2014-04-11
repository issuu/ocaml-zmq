let context = ZMQ.Context.create () in

let frontend = ZMQ.Socket.create context ZMQ.Socket.sub in
ZMQ.Socket.connect frontend "tcp://192.168.55.210:5556";

let backend = ZMQ.Socket.create context ZMQ.Socket.sub in
ZMQ.Socket.bind backend "tcp://10.1.1.0:8100";

ZMQ.Socket.subscribe frontend "";

while true do
  let finish = ref false in
  while not !finish do
    let message = ZMQ.Socket.recv frontend in
    if ZMQ.Socket.has_more frontend then
      ZMQ.Socket.send ~more:true backend message
    else begin
      ZMQ.Socket.send backend message;
      finish := true
    end
  done
done;

ZMQ.Socket.close frontend;
ZMQ.Socket.close backend;
ZMQ.Context.terminate context
