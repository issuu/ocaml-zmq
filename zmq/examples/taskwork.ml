(*
  Task worker
  Connects PULL socket to tcp://localhost:5557
  Collects workloads from ventilator via that socket
  Connects PUSH socket to tcp://localhost:5558
  Sends results to sink via that socket
*)

let ms_sleep msec =
  ignore (Unix.select [] [] [] (msec /. 1000.0))

let () =
  let module Socket = Zmq.Socket in
  let context = Zmq.Context.create () in

  (* Socket to receive messages on *)
  let receiver = Socket.create context Socket.pull in
  Socket.connect receiver "tcp://localhost:5557";

  (* Socket to send messages to *)
  let sender = Socket.create context Socket.push in
  Socket.connect sender "tcp://localhost:5558";

  (* Process tasks forever *)
  while true do
    let str = Socket.recv receiver in
    (* Simple progress indicator for the viewer *)
    flush stdout;
    Printf.printf "%s." str;

    (* Do the work *)
    ms_sleep (float_of_string str);

    (* Send results to sink *)
    Socket.send sender ""
  done;

  Socket.close receiver;
  Socket.close sender;
  Zmq.Context.terminate context
