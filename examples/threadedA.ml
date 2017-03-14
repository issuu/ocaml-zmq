let open ZMQ
                      
let subscription socket =
  let msg_from_b = Socket.recv ~block:true socket in
  print_endline @@ "msg from b:" ^ msg_from_b;
  subscription ~block topic socket 

let publish socket =
  Socket.send socket "thread A: Hello from A";
  Thread.delay 1.0
  publish socket
                      
let topic = "example"
let () =
  let context = Context.create () in

  let subscribe_thread =
    let subscribe_socket = Socket.create context Socket.sub in
    Socket.connect subscribe_socket "tcp://localhost:5000";
    Socket.subscribe subscribe_socket "thread B:";
    Thread.create subscription subscription_socket in
  
  let publish_thread =
    let publisher = Socket.create context Socket.pub in
    Socket.bind publisher "tcp://*:6000";
    Thread.create pushing push_socket in

  List.iter Thread.join [subscribe_thread; pushing_thread];

  List.iter Socket.close [subscribe_socket; push_socket];

  Context.terminate context

