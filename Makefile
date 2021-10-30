CFLAGS=-std=c18 -pedantic -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa)
LDLIBS=$(shell pkg-config --libs alsa) -lm
DEPDIR:=.deps
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$*.d
COMPILE.c=$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

EXECS=receiver sender
SRCS=receiver.c sender.c audio.c scheduling.c timing.c jb.c jb_table.c
OBJS=$(SRCS:%.c=%.o)

all: $(EXECS)

objs: $(OBJS)

receiver: audio.o receiver.o scheduling.o timing.o jb.o jb_table.o

sender: audio.o sender.o scheduling.o timing.o

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
