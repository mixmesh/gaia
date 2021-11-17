TARGETS=$(patsubst %.wav,%.s16,$(wildcard *.wav))

all:
	unzip -u $(wildcard *.zip); ./purge_filenames_in_cwd.sh; \
	$(MAKE) $(TARGETS)

%.s16 : %.wav
	sox $< -c 2 -r 44100 -b 16 -e signed-integer --endian little -t raw $@
