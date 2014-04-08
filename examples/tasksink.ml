(* 
	Task sink
	Binds PULL socket to tcp://localhost:5558
	Collects results from workers vis that socket
*)

let time_diff_ms t1 t2 = 
	truncate ((t2 -. t1) *. 1000.0) 

let () = 
	let module Socket = ZMQ.Socket in

	(* Prepare our context and socket *)
	let context = ZMQ.init () in 
	let receiver = Socket.create context Socket.pull in
	Socket.bind receiver "tcp://*:5558";

	(* Wait for start of batch *)
	let str = Socket.recv receiver

	(* Start our clock now *)
	let start_time = Unix.gettimeofday ();

	(* Process 100 confirmations *)
	for task_nbr = 0 to 99 do 
		let str = Socket.recv receiver in
		if task_nbr mod 10 == 0 then 
			Printf.printf ":"
		else 
			Printf.printf ".";
		flush stdout
	done;

	(* Calculate and report duration of batch *)
	Printf.printf "Total elapsed time: %d msec \n" time_diff start_time (Unix.gettimeofday ());

	Socket.close receiver;
	ZMQ.term context
