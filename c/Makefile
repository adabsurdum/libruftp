
STATICLIB=libruftp.a
UTIL=util

BUILD_ARCH=$(shell uname -m)

CPPFLAGS+=-D_GNU_SOURCE
# ...needed for struct timeval declaration.

ifdef DEBUG
CFLAGS+=-g
CPPFLAGS+=-D_DEBUG
else
CFLAGS+=-O3
CPPFLAGS+=-DNDEBUG
endif

CPPFLAGS+=-I$(UTIL)

CFLAGS+=-Wall -std=c99
# c99 necessary to use the inline keyword.
# But note from https://gcc.gnu.org/onlinedocs/gcc/Inline.html#Inline:
#  GCC does not inline any functions when not optimizing unless you specify
#  the ‘always_inline’ attribute for the function, like this: 
#  // Prototype.
#  inline void foo (const char) __attribute__((always_inline));

############################################################################

OBJECTS=packet.o \
	client.o \
	server.o \
	$(UTIL)/bvector.o \
	$(UTIL)/crc16.o \
	$(UTIL)/hexdump.o \
	$(UTIL)/socket.o \
	$(UTIL)/sockinit.o

############################################################################
# Rules

all : $(STATICLIB)

packet.o : packet.h \
	$(UTIL)/crc16.h \
	$(UTIL)/boolvect.h

client.o : client.h packet.h \
	$(UTIL)/crc16.h \
	$(UTIL)/boolvect.h \
	$(UTIL)/bvector.h \
	$(UTIL)/hexdump.h

server.o : server.h packet.h \
	datasrc.h \
	$(UTIL)/crc16.h \
	$(UTIL)/boolvect.h \
	$(UTIL)/bvector.h \
	$(UTIL)/hexdump.h

$(UTIL)/bvector.o $(UTIL)/crc16.o $(UTIL)/hexdump.o $(UTIL)/socket.o $(UTIL)/sockinit.o : util/libutil.a

# This merely triggers recursive compilation of all the util objects
# via the immediately preceding rule.

util/libutil.a : 
	$(MAKE) -C util

$(STATICLIB) : $(OBJECTS)
	$(AR) rcs $@ $^ 

############################################################################
# Unit tests

UNITTESTS=$(addprefix ut-,packet server)

allunit : $(UNITTESTS)

ut-packet : packet.c util/crc16.c
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) -DUNIT_TEST_PACKET=1 $^

ut-server : server.c util/crc16.c util/bvector.c
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) -DUNIT_TEST_SERVER=1 $^

############################################################################

clean : 
	$(MAKE) -C util clean
	rm -f *.o ut-* $(STATICLIB)

.PHONY : all allunit clean

