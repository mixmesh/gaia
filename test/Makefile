CFLAGS=-std=c18 -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm -lpthread

EXECS=capture display playback test_jb test_jb_table

all: $(EXECS)

run: test_jb test_jb_table
	./test_jb
	./test_jb_table

capture: capture.o ../audio.o
	(cd ..;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -o $@

playback: playback.o ../audio.o
	(cd ..;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -o $@

test_jb: test_jb.o ../jb.o
	(cd ..;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -o $@

test_jb_table: test_jb_table.o ../jb.o ../jb_table.o
	(cd ..;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -o $@

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #*

# FIXME: Manual dependencies

capture.o: capture.c ../audio.h

playback.o: playback.c ../audio.h 

test_jb.o: test_jb.c ../jb.h ../ ../bits.h

test_jb_table.o: test_jb_table.c ../jb_table.h
