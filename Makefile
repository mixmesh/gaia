CFLAGS=-std=c18 -Wpedantic -Werror
ALSA_CFLAGS=-g $(shell pkg-config --cflags alsa) -lm
LDLIBS =
ALSA_LDLIBS = $(shell pkg-config --libs alsa)

EXE=server client

all: $(EXE)

test:
	(cd test; $(MAKE))

# Executable files

server: jb.o jb_table.o server.o
	$(CC) $(LDLIBS) jb.o jb_table.o server.o -o server

client: jb.o client.o
	$(CC) $(LDLIBS) jb.o client.o -o client

# Object files

jb.o: jb.c jb.h
	$(CC) $(CFLAGS) -c jb.c

jb_table.o: jb.c jb.h jb_table.c jb_table.h
	$(CC) $(CFLAGS) -c jb_table.c

server.o: server.c jb.h jb_table.h
	$(CC) $(CFLAGS) -c server.c

client.o: client.c jb.h
	$(CC) $(CFLAGS) -c client.c

# Misc

clean:
	rm -f *.o $(EXE)

mrproper: clean
	rm -f *~ #* 
