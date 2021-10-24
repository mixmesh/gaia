CFLAGS=-std=c18 -pedantic -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm
#EXECS=server sender

EXECS=sender

all: $(EXECS)

test:
	(cd test; $(MAKE))

server: jb.o jb_table.o server.o audio.o timing.o

sender: jb.o sender.o audio.o timing.o

jb.o: jb.c jb.h bits.h

jb_table.o: jb_table.c jb.h

server.o: server.c jb.h jb_table.h bits.h timing.h

sender.o: sender.c jb.h audio.h timing.h

audio.o: audio.c audio.h

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #* 
