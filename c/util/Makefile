
#INCLUDE_CRYPTO=true
SKEIN_DIR=../../contrib/skein
BLOWFISH_DIR=../../contrib/blowfish

STATICLIB=libutil.a

BUILD_ARCH=$(shell uname -m)

ifdef DEBUG
CFLAGS+=-g
CPPFLAGS+=-D_DEBUG
else
CFLAGS+=-O3
CPPFLAGS+=-DNDEBUG
endif

CFLAGS+=-Wall -std=c99
# c99 necessary to use the inline keyword.
# But note from https://gcc.gnu.org/onlinedocs/gcc/Inline.html#Inline:
#  GCC does not inline any functions when not optimizing unless you specify
#  the ‘always_inline’ attribute for the function, like this: 
#  // Prototype.
#  inline void foo (const char) __attribute__((always_inline));

############################################################################

OBJECTS=bvector.o\
	crc16.o\
	hexdump.o\
	sockinit.o\
	socket.o

ifdef INCLUDE_CRYPTO
OBJECTS=crypto.o
endif

############################################################################
# Rules

all : $(STATICLIB)

crypto.o : crypto.c
	$(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) -I$(SKEIN_DIR) -I$(BLOWFISH_DIR) $<

# Following object dependencies use GNU make's implicit rules.

$(STATICLIB) : $(OBJECTS)
	$(AR) rcs $@ $^ 

############################################################################
# For future reference
# To statically link one library (e.g. libgsl.a) and dyanmic for remainder
#	$(CC) ... $^ -Wl,-Bstatic -L$(HOME)/lib -lgsl -Wl,-Bdynamic -lm -lpthread
############################################################################

############################################################################
# Unit tests

UNITTESTS=$(addprefix ut-,bvector crc16 socket crypto)

allunit : $(UNITTESTS)

ut-bvector : bvector.c
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) -DUNIT_TEST_BVECTOR=1 $^

ut-crc16 : crc16.c
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) -DUNIT_TEST_CRC16=1 $^

ut-socket : socket.c
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) -DUNIT_TEST_SOCKET=1 $^

ut-sockinit : sockinit.c
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) -DUNIT_TEST_SOCKINIT=1 $^

ut-crypto : crypto.c $(SKEIN_DIR)/libskein.a $(BLOWFISH_DIR)/libblowfish.a
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS)  -I$(SKEIN_DIR) -I$(BLOWFISH_DIR) -DUNIT_TEST_CRYPTO=1 $< -L$(SKEIN_DIR) -lskein -L$(BLOWFISH_DIR) -lblowfish

############################################################################

clean : 
	rm -f *.o ut-* $(STATICLIB)

.PHONY : all allunit clean

