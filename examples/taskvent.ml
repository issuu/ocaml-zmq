(*
    Task ventilator
    Binds PUSH socket to tcp://localhost:5557
    Sends batch of tasks to workers via that socket 
*)

(* build a list of size random numbers in [1, bound] *)
let random_list bound size = 
    let next_int () = (Random.int bound) + 1 in
    let rec loop i lst = 
        if i == size then lst else loop (i+1) ((next_int ()) :: lst) in
    loop 0 []

(* build a list of partial sum totals of the values in another list *)
let accum_list lst = 
    let update_accum ls x = match ls with [] -> [x] | y :: _ -> (x+y) :: ls in
    let revres = List.fold_left update_accum [] lst in
    match revres with
    | [] -> (0, [])
    | total :: _ -> (total, List.rev revres)

let () = 
    let module Socket = ZMQ.Socket in
    let context = ZMQ.init () in

    (* Socket to send messages on *)
    let sender = Socket.create context Socket.push in
    Socket.bind sender "tcp://*:5557"; 

    (* Socket to send start of batch message on *)
    let sink = Socket.create context Socket.push in
    Socket.connect sink "tcp://localhost:5558";

    Printf.printf "Press Enter when the workers are ready: ";
    ignore (read_line ());
    Printf.printf "Sending tasks to workers... \n";

    (* The first message is "0" and signals start of batch *)
    Socket.send sink "0";

    (* Initialize RNG with changing seed *)
    Random.self_init ();

    (* Send 100 tasks with random workload from 1 to 100 msec *)
    let workloads = random_list 100 100 in 
    let total, partials = accum_list workloads in
    List.iter (fun w -> let str = Printf.sprintf "%d" w in Socket.send sender str) partials;

    Printf.printf "Total expected cost: %d msec\n" total;
    Unix.sleep 1;              (* Give 0MQ time to deliver *)

    Socket.close sink;
    Socket.close sender;
    ZMQ.term context
