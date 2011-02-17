open Printf;;

let randint a b = (Random.int (b - a + 1)) + a;;

let context = ZMQ.init 1 in
let publisher = ZMQ.socket context ZMQ.Pub in
  ZMQ.bind publisher "tcp://*:5556";

  while true do
    let zipcode = (randint 1 10001) in
    let temperature = (randint 1 215) - 80 in
    let relhumidity = (randint 1 50) + 10 in
      ZMQ.send publisher (sprintf "%05d %d %d" zipcode temperature relhumidity) ZMQ.None
  done;

  ZMQ.close publisher;
  ZMQ.term context;

