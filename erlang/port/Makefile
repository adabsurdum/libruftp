
EROOT="$(HOME)/lib/erlang/lib/erl_interface-3.8"

CFLAGS=-std=c99
CPPFLAGS=-I$(EROOT)/include 

ifdef DEBUG
CPPFLAGS+=-O0 -g -D_DEBUG
else
CPPFLAGS+=-O3 -DNDEBUG
endif

snap : main.c image.c rw.c
	gcc -o $@ $(CPPFLAGS) $(CFLAGS) $^ -lpthread -L$(EROOT)/lib -lerl_interface -lei

install : snap
	install $< ../priv

clean :
	rm -f snap

.PHONY : install clean

