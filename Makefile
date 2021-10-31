CFLAGS=-std=c18 -pedantic -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm -lpthread
DEPDIR:=.deps
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$*.d
COMPILE.c=$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

EXECS=gaia receiver sender
SRCS=audio.c gaia.c jb.c jb_table.c network_receiver.c network_sender.c receiver.c sender.c timing.c
OBJS=$(SRCS:%.c=%.o)

all: $(EXECS)

objs: $(OBJS)

gaia: audio.o gaia.o jb.o jb_table.o network_receiver.o network_sender.o timing.o

receiver: audio.o network_receiver.o jb.o jb_table.o receiver.o timing.o

sender: audio.o network_sender.o sender.o timing.o

test:
	(cd test; $(MAKE))

%.o : %.c
%.o : %.c $(DEPDIR)/%.d | $(DEPDIR)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(DEPDIR): ; @mkdir -p $@

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d)
$(DEPFILES):

clean:
	rm -f *.o $(EXECS)

mrproper: clean
	rm -f *~ #* 

include $(wildcard $(DEPFILES))
