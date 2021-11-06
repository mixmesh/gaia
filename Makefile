CFLAGS=-std=c18 -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm -lpthread

DEPDIR:=.deps
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$*.d
COMPILE.c=$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

SRCS=audio.c audio_sink.c gaia.c jb.c jb_table.c network_receiver.c network_sender.c timing.c
OBJS=$(SRCS:%.c=%.o)

all: gaia

gaia: audio.o audio_sink.o gaia.o jb.o jb_table.o network_receiver.o network_sender.o timing.o

objs: $(OBJS)

test:
	(cd test; $(MAKE))

%.o : %.c
%.o : %.c $(DEPDIR)/%.d | $(DEPDIR)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(DEPDIR): ; @mkdir -p $@

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d)
$(DEPFILES):

clean:
	rm -f *.o gaia

mrproper: clean
	(cd test; $(MAKE) mrproper)
	rm -f *~ #* 

include $(wildcard $(DEPFILES))
