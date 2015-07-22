
STATICLIB=libblowfish.a

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

############################################################################

OBJECTS=blowfish.o

############################################################################
# Rules

all : $(STATICLIB)

# Following object dependencies use GNU make's implicit rules.

# Core modules.

blowfish.o  : blowfish.h

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

#UNITTESTS=$(addprefix ut-,video)

allunit : $(UNITTESTS)

#ut-video : video.c fourcc.c firstdev.c
#	$(CC) $(CFLAGS) -DUNIT_TEST_VIDEO=1 -o $@ -lX11 -lXpm $^

############################################################################

clean : 
	rm -f *.o ut-* $(STATICLIB)

.PHONY : all allunit clean

