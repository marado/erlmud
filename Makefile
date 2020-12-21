SHELL = /bin/bash
SRCS := $(wildcard src/*.erl)
OBJS := $(patsubst %.erl,%.beam,$(SRCS))

%.beam : %.erl
	erlc -o src $<
run : $(OBJS)
	erl -pa src -run mud start -s init stop
clean :
	rm $(OBJS)

PORT = $(shell ss -lp | grep beam.smp | sed -e 's/[^.]*.0.0.0:\([[:digit:]]*\).*/\1/')
test :
	echo -e "Hugo" | socat -dd - TCP:localhost:${PORT}
connect :
	socat -dd - TCP:localhost:${PORT}
