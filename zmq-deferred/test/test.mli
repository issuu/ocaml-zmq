module Make: functor(T: Zmq_deferred.Deferred.T) -> sig
  val run: ((unit -> unit T.Deferred.t) -> unit) -> unit
end
