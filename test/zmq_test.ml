open OUnit

open ZMQ
open ZMQ.Socket
open ZMQ.Poll

let debug fmt = 
  Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt 

let sleep t = ignore(Unix.select [] [] [] t)

let dump_events l = 
  let f  = function
    | None -> "None"
    | Some In -> "In"
    | Some Out -> " Out"
    | Some In_out -> "In/Out"
  in 
  let l = Array.to_list (Array.map f l) in
  "[|" ^ (String.concat "; " l) ^ "|]"

let suite = 
  "zmq test" >::: 
    [
      "request reply" >::
        (bracket
           (fun () ->
             let ctx = init () in
             let req = create ctx req 
             and rep = create ctx rep in
             ctx, req, rep
           )
           (fun (_, req, rep) -> 
             let endpoint = "inproc://endpoint" in
             bind rep endpoint;
             connect req endpoint;
             send req "request";
             let msg = recv rep in
             assert_equal "request" msg;
             send rep "reply";
             let msg = recv req in
             assert_equal "reply" msg
           )
           (fun (ctx, req, rep) ->
             close req;
             close rep;
             term ctx
           ));

      "poll" >::
        (bracket
           (fun () ->
             let ctx = init () in
             let req = create ctx req 
             and rep = create ctx rep in
             ctx, req, rep
           )
           (fun (_, req, rep) -> 
             let endpoint = "inproc://endpoint" in
             bind rep endpoint;
             connect req endpoint;
             let mask = mask_of [| req, In_out; rep, In_out |] in
             assert_equal [| Some Out; None |] (poll ~timeout:1000 mask);
             send req "request";
             assert_equal [| None; Some In |] (poll ~timeout:1000 mask);
             let msg = recv ~opt:R_no_block rep in
             assert_equal "request" msg;
             send rep "reply";
             assert_equal [| Some In; None |] (poll ~timeout:1000 mask);
             let msg = recv req in
             assert_equal "reply" msg;
           )
           (fun (ctx, req, rep) ->
             close req;
             close rep;
             term ctx
           ));
    ]
