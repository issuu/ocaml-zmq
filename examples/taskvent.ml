(*
    Task ventilator
    Binds PUSH socket to tcp://localhost:5557
    Sends batch of tasks to workers via that socket 
*)

let () = 
    let module Socket = ZMQ.Socket in
    let context = ZMQ.init () in
    let sender = Socket.create context Socket.push in
    Socket.bind sender "tcp://*:5557"; 

    let sink = Socket.create context Socket.push in
    Socket.connect sink "tcp://localhost:5558";

    Printf.printf "Press Enter when the workers are ready: ";
    ignore (read_line ());
    Printf.printf "Sending tasks to workers... \n";

    Socket.send sink "0";

    (* RNG *)
