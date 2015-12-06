
#include <stdlib.h>
#include <stdint.h>

#include "crc16.h"

static const uint16_t CRC16 = 0x8005;

static inline uint16_t brev16( uint16_t in ) {
	uint16_t res = 0;
	for(int i = 0x8000, j = 0x0001; i != 0; i >>=1, j <<= 1 )
		if( i & in ) res |= j;
	return res;
}


uint16_t crc16( const uint8_t *data, uint16_t size ) {

	uint16_t out = 0;
	int bits_read = 0, bit_flag;

	if( data == NULL )
		return 0;

	while( size > 0 ) {

		bit_flag = out >> 15;

		/* Get next bit: */
		out <<= 1;
		out |= (*data >> bits_read) & 1; // item a) work from the least significant bits

		/* Increment bit counter: */
		bits_read++;
		if(bits_read > 7) {
			bits_read = 0;
			data++;
			size--;
		}

		/* Cycle check: */
		if( bit_flag )
			out ^= CRC16;
	}

	// "push out" the last 16 bits

	for(int  i = 0; i < 16; ++i) {
		bit_flag = out >> 15;
		out <<= 1;
		if(bit_flag)
			out ^= CRC16;
	}

	return brev16(out);
}


#ifdef UNIT_TEST_CRC16

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int main( int argc, char *argv[] ) {

	struct stat info;
	uint16_t crc = 0;
	uint32_t len = 0;
	const char *target = argv[1];
	if( stat( target, &info ) == 0 ) {
		void *buf = malloc( info.st_size );
		FILE *fp = fopen( target, "r" );
		if( fp ) {
			fread( buf, sizeof(uint8_t), info.st_size, fp );
			fclose( fp );
		}
		len = info.st_size;
		crc = crc16( (const uint8_t *)buf, len );
	} else {
		len = strlen(target);
		crc = crc16( (const uint8_t *)target, len );
	}

	printf( "CRC16(%s) = %04x(%d)\n", target, crc, len );
	return EXIT_SUCCESS;
}
#endif

