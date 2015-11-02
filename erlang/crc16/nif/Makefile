
all : crc16_nif.so

crc16_nif.so : nif.c crc16.c
	$(CC) -o $@ -std=c99 -fpic -shared -I$(HOME)/lib/erlang/erts-7.0/include $^

clean :
	rm -f *.so

.PHONY : clean

