ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)

CFLAGS=-std=c18 -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm -lpthread -lrt -lopus

DEPDIR:=.deps
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$*.d
COMPILE.c=$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

SRCS=$(wildcard *.c)
OBJS=$(SRCS:%.c=%.o)

EXECS=gaia gaia_file_sender
NIFS=../priv/gaia_nif.so

all: $(EXECS) $(NIFS)

gaia: audio.o audio_sink.o gaia.o gaia_utils.o jb.o jb_table.o network_receiver.o network_sender.o timing.o

gaia_file_sender: file_sender.o gaia_file_sender.o gaia_utils.o timing.o

objs: $(OBJS)

test:
	(cd test; $(MAKE))

../priv/gaia_nif.so: audio_sink.o gaia_nif.o jb.o jb_table.o network_receiver.o timing.o
	$(CC) gaia_nif.o -shared $(LDLIBS) $(ERL_TOP)/usr/lib/libei.a -o ../priv/gaia_nif.so

gaia_nif.o: gaia_nif.c
	$(CC) $(CFLAGS) -I$(ERL_TOP)/usr/include -D_REENTRANT -fPIC -c gaia_nif.c

%.o : %.c
%.o : %.c $(DEPDIR)/%.d | $(DEPDIR)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(DEPDIR): ; @mkdir -p $@

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d)
$(DEPFILES):

clean:
	rm -f *.o $(EXECS) $(NIFS)

mrproper: clean
	(cd test; $(MAKE) mrproper)
	rm -f *~ #*

include $(wildcard $(DEPFILES))
