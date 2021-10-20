CFLAGS=-std=c18 -pedantic -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm
EXECS=test_jb test_jb_table display capture playback

all: $(EXECS)

run: test_jb test_jb_table
	./test_jb
	./test_jb_table

test_jb: test_jb.o
	(cd ..;$(MAKE))
	$(CC) test_jb.o ../jb.o $(LDLIBS) -o test_jb

test_jb_table: test_jb_table.o
	$(CC) test_jb_table.o ../jb.o ../jb_table.o $(LDLIBS) -o test_jb_table

capture: capture.o
	(cd ..;$(MAKE))
	$(CC) capture.o ../audio.o $(LDLIBS) -o capture

playback: playback.o
	(cd ..;$(MAKE))
	$(CC) playback.o ../audio.o $(LDLIBS) -o playback

test_jb.o: test_jb.c ../jb.h ../ ../bits.h

test_jb_table.o: test_jb_table.c ../jb_table.h

capture.o: capture.c ../audio.h

playback.o: playback.c ../audio.h 

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #*
