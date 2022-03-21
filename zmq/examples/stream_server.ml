
let ctx = Zmq.Context.create();;
let sock = Zmq.Socket.create ctx Zmq.Socket.stream;;
Zmq.Socket.bind sock "tcp://0.0.0.0:8089";;

while true do
  let l = Zmq.Socket.recv_all sock in
  Printf.printf "receive: [%s]\n%!" (String.concat ";" @@ List.map (Printf.sprintf "%S") l);
  match l with
  | [id; msg] when String.trim msg = "hello" ->
    Zmq.Socket.send_all sock [id; "world\n"]
  | _ -> ()
done;;

Zmq.Socket.close sock;
Zmq.Context.terminate ctx
