open List;;
open ZMQ;;

let split = Str.split (Str.regexp_string " ");;

let context = ZMQ.init () in
let subscriber = Socket.create context Socket.sub in
print_endline "Collecting updates from weather server...";
Socket.connect subscriber "tcp://localhost:5556";

let filter = if (Array.length Sys.argv) > 1 then Sys.argv.(1) else "10001 " in
Socket.subscribe subscriber filter;

let total_temp = ref 0 in
let update_nbr = 100 in
for i = 0 to pred update_nbr do
  let str = Socket.recv subscriber in
  match map int_of_string (split str) with
  | [ _; temperature; _] -> total_temp := !total_temp + temperature;
  | _ -> ();
done;

Printf.printf "Average temperature for zipcode %s was %d\n" filter (!total_temp / update_nbr);

Socket.close subscriber;
ZMQ.term context
