CFLAGS=-std=c18 -pedantic -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm
EXECS=server client

all: $(EXECS)

test:
	(cd test; $(MAKE))

server: jb.o jb_table.o server.o audio.o timing.o

client: jb.o client.o audio.o timing.o

jb.o: jb.c jb.h bits.h

jb_table.o: jb_table.c jb.h

server.o: server.c jb.h jb_table.h bits.h timing.h

client.o: client.c jb.h audio.h timing.h

audio.o: audio.c audio.h

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #* 
