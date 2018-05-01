let split = Str.split (Str.regexp_string " ")

let () =
  let context = Zmq.Context.create () in
  let subscriber = Zmq.Socket.create context Zmq.Socket.sub in
  print_endline "Collecting updates from weather server...";
  Zmq.Socket.connect subscriber "tcp://localhost:5556";

  let filter = if (Array.length Sys.argv) > 1 then Sys.argv.(1) else "10001 " in
  Zmq.Socket.subscribe subscriber filter;

  let total_temp = ref 0 in
  let update_nbr = 100 in
  for _i = 0 to pred update_nbr do
    let str = Zmq.Socket.recv subscriber in
    match List.map int_of_string (split str) with
    | [ _; temperature; _] -> total_temp := !total_temp + temperature;
    | _ -> ();
  done;

  Printf.printf "Average temperature for zipcode %s was %d\n" filter (!total_temp / update_nbr);

  Zmq.Socket.close subscriber;
  Zmq.Context.terminate context
