SHELL = /bin/bash
run :
	cd src ; erl -noshell -eval 'compile:file(mud)' -run mud start -s init stop
PORT = $(shell ss -lp | grep beam.smp | sed -e 's/[^.]*.0.0.0:\([[:digit:]]*\).*/\1/')
test :
	echo -e "Hugo" | socat -dd - TCP:localhost:${PORT}
connect :
	socat -dd - TCP:localhost:${PORT}
