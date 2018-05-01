open Zmq

let rec subscription socket =
  (* Block thread until there is data *)
  let msg = Socket.recv socket in
  let thread = Thread.self () |> Thread.id |> string_of_int in
  print_endline @@ "thread " ^ thread ^ " received: " ^ msg;
  subscription socket

let rec publish count socket =
  let thread = Thread.self () |> Thread.id |> string_of_int in
  let msg = "Hello #" ^ string_of_int count ^ " from thread " ^ thread in
  Socket.send socket msg;
  print_endline @@ "thread " ^ thread ^ " sent: " ^ msg;
  Thread.delay 1.0;
  publish (succ count) socket

let () =
  let context = Context.create () in

  let publish_socket = Socket.create context Socket.pub in
  let publish_thread =
    Socket.bind publish_socket "tcp://*:5000";
    Thread.create (publish 0) publish_socket in

  let subscribe_socket = Socket.create context Socket.sub in
  let subscribe_thread =
    Socket.connect subscribe_socket "tcp://127.0.0.1:5000";
    Socket.subscribe subscribe_socket "";
    Thread.create subscription subscribe_socket in

  List.iter Thread.join  [subscribe_thread; publish_thread];
  List.iter Socket.close [subscribe_socket; publish_socket];

  Context.terminate context
