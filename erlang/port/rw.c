
#include <stdlib.h>
#include <unistd.h>
#include "rw.h"

int read_exact( byte *buf, int len ) {

	int i, rd = 0;

	do {
		if( (i = read(0, buf+rd, len-rd)) <= 0 )
			return i;
		rd += i;
	} while( rd < len );

	return len;
}

int write_exact( const byte *buf, int len ) {
	int i, wr = 0;

	do {
		if( (i = write(1, buf+wr, len-wr)) <= 0 )
			return i;
		wr += i;
	} while( wr < len );

	return len;
}


int read_lpm( byte *mbuf, int mlen ) {
	unsigned int required_len;
	byte lbuf[4];
	read_exact( lbuf, 4 );
	required_len 
		= ( 0xFF000000 & (lbuf[0] << 24))
		| ( 0x00FF0000 & (lbuf[1] << 16))
		| ( 0x0000FF00 & (lbuf[2] <<  8))
		| ( 0x000000FF & (lbuf[3] <<  0));
	if( required_len <= mlen )
		return read_exact( mbuf, required_len );
	else
		return -required_len;
}


int write_lpm( const byte *mbuf, int mlen ) {
	const byte LBUF[4] = {
		(byte)((mlen >> 24) & 0xFF),
		(byte)((mlen >> 16) & 0xFF),
		(byte)((mlen >>  8) & 0xFF),
		(byte)((mlen >>  0) & 0xFF)
	};
	if( write_exact( LBUF, sizeof(LBUF) ) == sizeof(LBUF) )
		return write_exact( mbuf, mlen );
	else
		return 0;
}

