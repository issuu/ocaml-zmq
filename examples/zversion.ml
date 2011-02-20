let _ =
  let (x, y, z) = ZMQ.version () in
  Printf.printf "Current 0MQ version is %d.%d.%d\n" x y z
