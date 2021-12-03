ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
CFLAGS=-std=c18 -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa) -I$(ERL_TOP)/usr/include -I ../c_src
LDLIBS=$(shell pkg-config --libs alsa) $(ERL_TOP)/usr/lib/libei.a -lm -lpthread -lrt

EXECS=capture display opus_example playback test_jb test_jb_table

all: $(EXECS)

run: test_jb test_jb_table
	./test_jb
	./test_jb_table

capture: capture.o ../c_src/audio.o
	(cd ../c_src;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -o $@

opus_example: opus_example.o
	$(CC) $^ $(LDLIBS) -l opus -o $@

playback: playback.o ../c_src/audio.o
	(cd ../c_src;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -o $@

test_jb: test_jb.o ../c_src/jb.o ../c_src/threadlib.o
	(cd ../c_src;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -lopus -o $@

test_jb_table: test_jb_table.o ../c_src/jb.o ../c_src/jb_table.o ../c_src/threadlib.o
	(cd ../c_src/;$(MAKE) objs)
	$(CC) $^ $(LDLIBS) -lopus -o $@

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #*

# FIXME: Manual dependencies

capture.o: capture.c ../c_src/audio.h

playback.o: playback.c ../c_src/audio.h

test_jb.o: test_jb.c ../c_src/jb.h ../c_src/bits.h

test_jb_table.o: test_jb_table.c ../c_src/jb_table.h
