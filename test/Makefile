CFLAGS=-std=c18 -pedantic -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm
EXECS=test_jb test_jb_table display simple_recording simple_playback

all: $(EXECS)

run: test_jb test_jb_table
	./test_jb
	./test_jb_table

test_jb: test_jb.o
	(cd ..;$(MAKE))
	$(CC) test_jb.o ../jb.o $(LDLIBS) -o test_jb

test_jb_table: test_jb_table.o
	$(CC) test_jb_table.o ../jb.o ../jb_table.o $(LDLIBS) -o test_jb_table

simple_recording: simple_recording.o
	(cd ..;$(MAKE))
	$(CC) simple_recording.o ../audio.o $(LDLIBS) -o simple_recording

simple_playback: simple_playback.o
	(cd ..;$(MAKE))
	$(CC) simple_playback.o ../audio.o $(LDLIBS) -o simple_playback

test_jb.o: test_jb.c ../jb.h ../ ../bits.h

test_jb_table.o: test_jb_table.c ../jb_table.h

simple_recording.o: simple_recording.c ../audio.h 

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #*
