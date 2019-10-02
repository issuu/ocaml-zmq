open OUnit

(* random key generated with `curve_keygen` *)
let z85_public = "^fh#binNS?sWfA=FHY#Rw2b{1{1kzB+Vr.kZzIAA"
let z85_secret = "E9.KnISxjbj*R<q&Q}T69x9IvxKNwUK3q*lv5^)P"

let test_set_get_key () =
  let assert_equal = assert_equal ~printer:(fun x -> x) in
  let ctx = Zmq.Context.create () in
  let sock = Zmq.Socket.create ctx Zmq.Socket.pub in
  Zmq.Socket.set_curve_publickey sock z85_public;
  let pub = Zmq.Socket.get_curve_publickey sock in
  assert_equal ~msg:"Matching public key" z85_public pub;
  Zmq.Socket.set_curve_secretkey sock z85_secret;
  let sec = Zmq.Socket.get_curve_secretkey sock in
  assert_equal ~msg:"Matching private key" z85_secret sec;
  Zmq.Socket.set_curve_serverkey sock z85_public;
  let servpub = Zmq.Socket.get_curve_serverkey sock in
  assert_equal ~msg:"Matching server key" z85_public servpub;

  Zmq.Socket.close sock;
  Zmq.Context.terminate ctx

let suite = "curve" >::: [
  "set/get key" >:: test_set_get_key
]
