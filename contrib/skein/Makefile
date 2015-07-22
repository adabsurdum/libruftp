
STATICLIB=libskein.a

ifdef DEBUG
CFLAGS+=-g
CPPFLAGS+=-D_DEBUG
else
CFLAGS+=-O3
CPPFLAGS+=-DNDEBUG
endif

CFLAGS+=-Wall -std=c99 -Wno-strict-aliasing

# c99 necessary to use the inline keyword.
# But note from https://gcc.gnu.org/onlinedocs/gcc/Inline.html#Inline:
#  GCC does not inline any functions when not optimizing unless you specify
#  the ‘always_inline’ attribute for the function, like this: 
#  // Prototype.
#  inline void foo (const char) __attribute__((always_inline));

# The resulting executable will only monitor one video device per process.

#CFLAGS += -D_POSIX_C_SOURCE=200809L
# _POSIX_SOURCE sufficient for localtime_r.
# getline requires _POSIX_C_SOURCE=200809L.

############################################################################

OBJECTS=skein.o skein_block.o

############################################################################
# Rules

all : $(STATICLIB)

# Following object dependencies use GNU make's implicit rules.

# Core modules.

skein.o  : skein.h skein_port.h brg_types.h brg_endian.h skein_iv.h
skein_block.o : skein.h skein_port.h brg_types.h brg_endian.h 

# Helper/accessory modules

$(STATICLIB) : $(OBJECTS)
	$(AR) rcs $@ $^ 

############################################################################
# For future reference
# To statically link one library (e.g. libgsl.a) and dyanmic for remainder
#	$(CC) ... $^ -Wl,-Bstatic -L$(HOME)/lib -lgsl -Wl,-Bdynamic -lm -lpthread
############################################################################

############################################################################
# Unit tests

############################################################################

clean : 
	rm -f *.o ut-* $(STATICLIB)

.PHONY : all allunit clean

