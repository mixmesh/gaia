CFLAGS=-Wpedantic -Werror
ALSA_CFLAGS=-g $(shell pkg-config --cflags alsa) -lm
LDLIBS =
ALSA_LDLIBS = $(shell pkg-config --libs alsa)

EXE=test_jb test_jb_table

all: $(EXE)

run: test_jb test_jb_table
	./test_jb
	./test_jb_table

# Executable files

test_jb: test_jb.o
	$(CC) $(LDLIBS) ../jb.o test_jb.o -o test_jb

test_jb_table: test_jb_table.o
	$(CC) $(LDLIBS) ../jb.o ../jb_table.o test_jb_table.o -o test_jb_table

# Object files

test_jb.o: test_jb.c ../jb.h ../ ../bits.h
	(cd ..;$(MAKE))
	$(CC) $(CFLAGS) -c test_jb.c

test_jb_table.o: test_jb_table.c ../jb.h ../jb_table.h ../ ../bits.h
	(cd ..;$(MAKE))
	$(CC) $(CFLAGS) -c test_jb_table.c

# Misc

clean:
	rm -f *.o $(EXE)

mrproper: clean
	rm -f *~ #*
