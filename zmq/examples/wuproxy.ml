let context = Zmq.Context.create () in

let frontend = Zmq.Socket.create context Zmq.Socket.sub in
Zmq.Socket.connect frontend "tcp://192.168.55.210:5556";

let backend = Zmq.Socket.create context Zmq.Socket.sub in
Zmq.Socket.bind backend "tcp://10.1.1.0:8100";

Zmq.Socket.subscribe frontend "";

while true do
  let finish = ref false in
  while not !finish do
    let message = Zmq.Socket.recv frontend in
    if Zmq.Socket.has_more frontend then
      Zmq.Socket.send ~more:true backend message
    else begin
      Zmq.Socket.send backend message;
      finish := true
    end
  done
done;

Zmq.Socket.close frontend;
Zmq.Socket.close backend;
Zmq.Context.terminate context
