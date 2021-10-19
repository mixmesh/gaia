CFLAGS=-g $(shell pkg-config --cflags alsa) -lm
LDLIBS=$(shell pkg-config --libs alsa)
EXECS=test_jb test_jb_table display simple_playback simple_recording

all: $(EXECS)

run: test_jb test_jb_table
	./test_jb
	./test_jb_table

test_jb: test_jb.o
	(cd ..;$(MAKE))
	$(CC) $(LDLIBS) test_jb.o ../jb.o -o test_jb

test_jb_table: test_jb_table.o
	$(CC) $(LDLIBS) test_jb_table.o ../jb.o ../jb_table.o -o test_jb_table

test_jb.o: test_jb.c ../jb.h ../ ../bits.h

test_jb_table.o: test_jb_table.c ../jb_table.h

clean:
	rm -f *.o $(EXE)

mrproper: clean
	rm -f *~ #*
