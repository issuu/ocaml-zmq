module C = Configurator.V1

let () =
  C.main ~name:"zmq" (fun c ->
      let default : C.Pkg_config.package_conf = {
        libs = ["-lzmq"];
        cflags = []
      } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
          begin match C.Pkg_config.query pc ~package:"libzmq" with
            | Some s -> s
            | None -> default
          end
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
