open OUnit
module Zmq = ZMQ

open Lwt.Infix
module Socket = struct
  include Zmq_lwt.Socket
  let close t =
    to_socket t |> Zmq.Socket.close;
    Lwt.return ()
end

module Deferred = struct
  include Lwt
  let sleepf = Lwt_unix.sleep
  let don't_wait_for = Lwt.async
  let catch f = Lwt_result.catch (f ())
end

let list_init cnt f =
  let rec loop = function
    | n when n = cnt -> []
    | n -> f n :: loop (n + 1)
  in
  loop 0 |> List.rev

let all_ok l =  List.fold_left (fun acc a -> acc >>= fun () -> a) (Deferred.return ()) l

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
  Deferred.sleepf 0.0001 >>= fun () ->
  Deferred.return (ctx, Socket.of_socket s1, Socket.of_socket s2)

let teardown (ctx, s1, s2) =
  Socket.close s2 >>= fun () ->
  Socket.close s1 >>= fun () ->
  Zmq.Context.terminate ctx;
  Deferred.return ()

let rec send ?delay s = function
  | 0 -> Deferred.return ()
  | n ->
    Socket.send s "test" >>= fun _ ->
    begin
      match delay with
      | None -> Deferred.return ()
      | Some delay -> Deferred.sleepf delay
    end >>= fun () ->
    send s (n - 1)

let rec recv ?delay s = function
  | 0 -> Deferred.return ()
  | n ->
    Socket.recv s >>= fun _ ->
    begin
      match delay with
      | None -> Deferred.return ()
      | Some delay -> Deferred.sleepf delay
    end >>= fun () ->
    recv s (n - 1)

let test_simple (_, s1, s2) =
  all_ok [ send s1 1; recv s2 1 ]

(* Tests *)
let test_send_receive (_, s1, s2) =
  all_ok [
    send s2 100; recv s1 100
  ]

let test_msend_mreceive (_, s1, s2) =
  all_ok [
    send s2 100; send s2 100; send s2 100; send s2 100;
    recv s1 100; recv s1 100; recv s1 100; recv s1 100;
  ]

let test_mix (_, s1, s2) =
  all_ok [
    send s2 100; recv s1 100;
    send s1 100; recv s2 100;
    send s2 100; recv s1 100;
    send s1 100; recv s2 100;
  ]

let test_slow_send (_, s1, s2) =
  all_ok [
    recv ~delay:0.001 s2 100;
    send s1 20;
    send s1 20;
    send s1 20;
    send s1 20;
    send s1 20;
  ]

let test_slow_receive (_, s1, s2) =
  all_ok [
    send ~delay:0.001 s2 100;
    recv s1 20;
    recv s1 20;
    recv s1 20;
    recv s1 20;
    recv s1 20;
  ]

let test_multi (_, s1, s2) =
  all_ok (
    ((send ~delay:0.001 s1 100) :: (list_init 100 (fun _ -> Socket.recv s2 >>= fun _ -> Deferred.return ())))
    @
    ((send ~delay:0.002 s2 100) :: (list_init 100 (fun _ -> Socket.recv s1 >>= fun _ -> Deferred.return ())))
  )

let test_slow_mix (_, s1, s2) =
  all_ok [
    send ~delay:0.001 s2 100; recv ~delay:0.001 s1 100;
    send ~delay:0.001 s1 100; recv ~delay:0.001 s2 100;
    send ~delay:0.001 s2 100; recv ~delay:0.001 s1 100;
    send ~delay:0.001 s1 100; recv ~delay:0.001 s2 100;
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

let () =
  let exec f = Lwt_main.run (f ()) in
  run_test_tt_main (suite exec) |> ignore
