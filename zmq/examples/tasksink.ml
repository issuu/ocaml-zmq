(*
  Task sink
  Binds PULL socket to tcp://localhost:5558
  Collects results from workers vis that socket
*)

let time_diff_ms t1 t2 =
  truncate ((t2 -. t1) *. 1000.0)

let () =
  let module Socket = Zmq.Socket in

  (* Prepare our context and socket *)
  let context = Zmq.Context.create () in
  let receiver = Socket.create context Socket.pull in
  Socket.bind receiver "tcp://*:5558";

  (* Wait for start of batch *)
  let _ = Socket.recv receiver in

  (* Start our clock now *)
  let start_time = Unix.gettimeofday () in

  (* Process 100 confirmations *)
  for task_nbr = 0 to 99 do
    let _ = Socket.recv receiver in
    if task_nbr mod 10 == 0 then
      Printf.printf ":"
    else
      Printf.printf ".";
    flush stdout
  done;

  (* Calculate and report duration of batch *)
  let elapsed = time_diff_ms start_time (Unix.gettimeofday ()) in
  Printf.printf "Total elapsed time: %d msec \n" elapsed;

  Socket.close receiver;
  Zmq.Context.terminate context
