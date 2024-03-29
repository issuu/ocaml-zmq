open OUnit
let count = 1000

let list_init cnt f =
  let rec loop = function
    | n when n = cnt -> []
    | n -> f n :: loop (n + 1)
  in
  loop 0 |> List.rev

module Make(T: Zmq_deferred.Deferred.T) = struct
  open T
  open Deferred.Infix

  module Socket = Zmq_deferred.Socket.Make(T)

  let all_ok l =  List.fold_left (fun acc a -> acc >>= fun () -> a) (T.Deferred.return ()) l
  let setup () =
    let make ctx tpe =
      let s = Zmq.Socket.create ctx tpe in
      Zmq.Socket.set_receive_high_water_mark s 1;
      Zmq.Socket.set_send_high_water_mark s 2;
      s
    in
    let ctx = Zmq.Context.create () in
    let s1 = make ctx Zmq.Socket.pair in
    let s2 = make ctx Zmq.Socket.pair in
    let endpoint = "inproc://test"  in
    Zmq.Socket.bind s1 endpoint;
    Zmq.Socket.connect s2 endpoint;
    T.Deferred.sleepf 0.0001 >>= fun () ->
    T.Deferred.return (ctx, Socket.of_socket s1, Socket.of_socket s2)

  let teardown (ctx, s1, s2) =
    Socket.close s2 >>= fun () ->
    Socket.close s1 >>= fun () ->
    Zmq.Context.terminate ctx;
    T.Deferred.return ()

  let rec send ?(delay = 0.0) s = function
    | 0 -> T.Deferred.return ()
    | n ->
      Socket.send s "test" >>= fun () ->
      T.Deferred.sleepf delay >>= fun () ->
      send s ~delay (n - 1)

  let rec recv ?(delay = 0.0) s = function
    | 0 -> T.Deferred.return ()
    | n ->
      Socket.recv s >>= fun _ ->
      T.Deferred.sleepf delay >>= fun () ->
      recv s ~delay (n - 1)

  (* Tests *)
  let test_send_receive (_, s1, s2) =
    all_ok [
      send s2 count; recv s1 count
    ]

  let test_msend_mreceive (_, s1, s2) =
    all_ok [
      send s2 count; send s2 count; send s2 count; send s2 count;
      recv s1 count; recv s1 count; recv s1 count; recv s1 count;
    ]

  let test_mix (_, s1, s2) =
    all_ok [
      send s2 count; recv s1 count;
      send s1 count; recv s2 count;
      send s2 count; recv s1 count;
      send s1 count; recv s2 count;
      send s2 count; recv s1 count;
    ]

  let test_slow_send (_, s1, s2) =
    all_ok [
      recv ~delay:0.0001 s2 count;
      send s1 (count / 5);
      send s1 (count / 5);
      send s1 (count / 5);
      send s1 (count / 5);
      send s1 (count / 5);
    ]

  let test_slow_receive (_, s1, s2) =
    all_ok [
      send ~delay:0.0001 s2 count;
      recv s1 (count / 5);
      recv s1 (count / 5);
      recv s1 (count / 5);
      recv s1 (count / 5);
      recv s1 (count / 5);
    ]

  let test_multi (_, s1, s2) =
    all_ok (
      ((send ~delay:0.0001 s1 count) :: (list_init count (fun _ -> Socket.recv s2 >>= fun _ -> Deferred.return ())))
      @
      ((send ~delay:0.0002 s2 count) :: (list_init count (fun _ -> Socket.recv s1 >>= fun _ -> Deferred.return ())))
    )

  let test_slow_mix (_, s1, s2) =
    all_ok [
      send ~delay:0.0001 s2 count; recv ~delay:0.0002 s1 count;
      send ~delay:0.0001 s1 count; recv ~delay:0.0002 s2 count;
      send ~delay:0.0001 s2 count; recv ~delay:0.0002 s1 count;
      send ~delay:0.0001 s1 count; recv ~delay:0.0002 s2 count;
    ]

  let suite (exec : (unit -> unit Deferred.t) -> unit) =
    let bracket setup test teardown =
      let f () =
        setup () >>= fun v ->
        Deferred.catch (fun () -> test v) >>= fun r ->
        teardown v >>= fun () ->
        match r with
        | Ok v -> Deferred.return v
        | Error exn -> Deferred.fail exn
      in
      fun () -> exec f
    in

    __MODULE__ >::: [
      "test_send_receive"   >:: bracket setup test_send_receive teardown;
      "test_msend_mreceive" >:: bracket setup test_msend_mreceive teardown;
      "test_mix"            >:: bracket setup test_mix teardown;
      "test_slow_send"      >:: bracket setup test_slow_send teardown;
      "test_slow_receive"   >:: bracket setup test_slow_receive teardown;
      "test_slow_mix"       >:: bracket setup test_slow_mix teardown;
      "test_multi"          >:: bracket setup test_multi teardown;
    ]

  let run exec =
     run_test_tt_main (suite exec) |> ignore

end
