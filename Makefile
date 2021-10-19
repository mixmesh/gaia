CFLAGS=-g $(shell pkg-config --cflags alsa) -lm
LDLIBS=$(shell pkg-config --libs alsa)
EXECS=server client

all: $(EXECS)

test:
	(cd test; $(MAKE))

server: jb.o jb_table.o server.o

client: jb.o client.o audio.o timing.o

jb.o: jb.c jb.h bits.h globals.h

jb_table.o: jb_table.c jb.h

server.o: server.c jb.h jb_table.h globals.h bits.h

client.o: client.c jb.h globals.h audio.h timing.h

audio.o: audio.c audio.h

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #* 
