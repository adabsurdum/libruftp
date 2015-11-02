
#ifndef _rw_h_
#define _rw_h_

typedef unsigned char byte;

// All return a value <= 0 on failure, or a positive count on success.

int read_exact( byte *buf, int len );
int write_exact( const byte *buf, int len );

// Following handle prefixing 2-byte lengths to the messages.
// ("lpm" stands for Length Prefixed Message.)

int read_lpm( byte *mbuf, int mlen );
int write_lpm( const byte *mbuf, int mlen );

#endif

