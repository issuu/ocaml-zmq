open List;;

let split = Str.split (Str.regexp_string " ");;

let context = ZMQ.init 1 in
let subscriber = ZMQ.socket context ZMQ.Sub in

print_endline "Collecting updates from weather server...";
ZMQ.connect subscriber "tcp://localhost:5556";

let filter = if (Array.length Sys.argv) > 1 then Sys.argv.(1) else "10001 " in
ZMQ.setsockopt subscriber (`Subscribe(filter));

let total_temp = ref 0 in
let update_nbr = 100 in
for i = 0 to pred update_nbr do
  let str = ZMQ.recv subscriber ZMQ.None in
  Printf.printf "[%s] leng %d\n%!" str (length (split str));
  Printf.printf "%d\n%!" (int_of_string (nth (split str) 0 ));
  Printf.printf "%d\n%!" (int_of_string (nth (split str) 1 ));
  Printf.printf "%d\n%!" (String.length (nth (split str) 2));
  Printf.printf "|%s|\n%!" (nth (split str) 2);
  Printf.printf "%d\n%!" (int_of_string (nth (split str) 2 ));
  match map int_of_string (split str) with
  | [ _; temperature; _] -> total_temp := !total_temp + temperature;
  | _ -> ();
done;

Printf.printf "Average temperature for zipcode %s was %d\n" filter (!total_temp / update_nbr);

ZMQ.close subscriber;
ZMQ.term context

