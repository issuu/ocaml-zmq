open ZMQ;;

let randint a b = (Random.int (b - a + 1)) + a;;

let context = ZMQ.Context.create () in
let publisher = ZMQ.Socket.create context ZMQ.Socket.pub in
ZMQ.Socket.bind publisher "tcp://*:5556";

while true do
  let zipcode = (randint 1 10001) in
  let temperature = (randint 1 215) - 80 in
  let relhumidity = (randint 1 50) + 10 in
  Socket.send publisher (Printf.sprintf "%05d %d %d" zipcode temperature relhumidity)
done;

ZMQ.Socket.close publisher;
ZMQ.Context.terminate context;
