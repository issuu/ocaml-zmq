default all:
	@echo "==== Building ocaml-zmq ===="	
	$(MAKE) -C src all
	@echo "==== Successfully built ocaml-zmq ===="	

install: all
	@echo "==== Installing ocaml-zmq ===="
	$(MAKE) -C src install
	@echo "==== Successfully installed ocaml-zmq ===="

uninstall:
	@echo "==== Uninstalling ocaml-zmq ===="
	$(MAKE) -C src uninstall
	@echo "==== Successfully uninstalled ocaml-zmq ===="

reinstall:
	@echo "==== Reinstalling ocaml-zmq ===="
	$(MAKE) -C src reinstall
	@echo "==== Successfully reinstalled ocaml-zmq ===="

clean:
	$(MAKE) -C src clean 

