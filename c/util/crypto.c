
/**
  * Copyright (C) 2015  Roger Kramer
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <arpa/inet.h> // for htonl
#include <unistd.h>
#include <assert.h>
#include <err.h>

#include "skein.h"
#include "blowfish.h"

#include "crypto.h"

#define HASHBITS     (512)
#define SIZEOF_HASH  ((HASHBITS)/8)

#define BLOCKBITS    (HASHBITS/2)
#define SIZEOF_BLOCK ((BLOCKBITS)/8)

#define MINLEN_KEY   (1)
#define MAXLEN_KEY	 (63)

static Skein_512_Ctxt_t _skein_context;
static uint8_t _xorpad[ SIZEOF_HASH ];

static BLOWFISH_CTX _blowfish_context;

static uint8_t _key[ MAXLEN_KEY+1 ];
static int     _key_len;

#ifdef _DEBUG
static int _bit_count( uint8_t *buf, int len ) {

	int n = 0;
	while( len-- > 0 )
		n += __builtin_popcount( 0x000000FF & (*buf++) );
	return n;
}


static void _report_bias( uint8_t *buf, int len, FILE *fp ) {
	const int setbits
		= _bit_count( buf, len );
	fprintf( fp, "%d / %d (=%.1f%%)\n",
		setbits, len * 8, (100.0*setbits)/(8.0*len) );
}
#endif

/**
  * Iterate the encrypt/decrypt functions over a size-aligned buffer.
  */
static void _map(
	void (*cb)( BLOWFISH_CTX *ctx, unsigned int *, unsigned int *),
	void *ctx, uint8_t *plaintext, int len ) {

	assert( len % 8 == 0 );
	uint32_t *xl = (uint32_t *)(plaintext+0);
	uint32_t *xr = (uint32_t *)(plaintext+4);
	while( len > 0 ) {
		cb( ctx, (unsigned int*)xl, (unsigned int*)xr );
		xl += 2;
		xr += 2;
		len -= 8;
	}
}


/**
  * XOR a (plain|cipher) text with a Skein hash-based one time pad.
  */
int _xorotp( uint8_t *iv, int ivlen, uint8_t *buf, int len ) {

	int rem = 0;
	uint8_t *p = buf;

	Skein_512_Init( &_skein_context, HASHBITS );
	// Key...
	Skein_512_Update( &_skein_context, _key, _key_len );
	// ...then IV.
	Skein_512_Update( &_skein_context, iv, ivlen );

	while( len-- > 0 ) {
		if( rem < 1 ) {
			// Lower half of SIZEOF_HASH becomes part of OTP, and...
			Skein_512_Final( &_skein_context, _xorpad );
			// ...upper half is recycled to generate next pad value.
			Skein_512_Update( &_skein_context, _xorpad + SIZEOF_BLOCK, SIZEOF_BLOCK );
			rem = SIZEOF_BLOCK;
		}
		*p ^= _xorpad[ SIZEOF_BLOCK - rem];
		p++;
		rem --;
	}
	return 0;
}


int crypto_setkey( const uint8_t *key, int len ) {

	if( key == NULL || len < MINLEN_KEY )
		return -1;
	if( len > MAXLEN_KEY )
		return MAXLEN_KEY;

	_key_len = len;
	memset( _key, 0, sizeof(_key) );
	memcpy( _key, key, _key_len );
	return 0;
}


/**
  * Blowfish (and other ciphers) require data length to be a multiple of
  * their block lengths, and because the payload may be binary receiver
  * needs to know how much of the total length is actual content, so I'm
  * prefixing the content with byte length.
  */
int crypto_sizeof_ciphertext( int payload ) {
	payload += 4; // for size prefix
	return (payload % 8) > 0
		? payload + 8 - (payload % 8)
		: payload;
}


/**
  * 1. Loads the given file into the provided buffer,
  * 2. Blowfish encrypts it, then
  * 3. Skein hashes the result to unbias it.
  */
int crypto_encrypt( FILE *fp, int plaintextlen, uint8_t *iv, int ivlen, uint8_t *buf, int buflen ) {

	const int CLEN
		= crypto_sizeof_ciphertext( plaintextlen );

	Blowfish_Init( &_blowfish_context, _key, _key_len );

	if( CLEN <= buflen ) {
		const int rd
			= fread( buf + sizeof(uint32_t), sizeof(uint8_t), plaintextlen, fp );
		if( rd == plaintextlen ) {
			*((uint32_t*)buf) = htonl(plaintextlen);
			_map( Blowfish_Encrypt, &_blowfish_context, buf, CLEN );
			_xorotp( iv, ivlen, buf, CLEN );
		} else {
			warn( "%s: reading file", __func__ );
			return -1;
		}
	} else {
		warnx( "%s: %d-byte buffer too small; need %d", __func__, buflen, CLEN );
		return -1;
	}

	return 0;
}


int crypto_decrypt( uint8_t *iv, int ivlen, uint8_t *buf, int buflen ) {
	uint32_t len;

	Blowfish_Init( &_blowfish_context, _key, _key_len );

	_xorotp( iv, ivlen, buf, buflen );
	_map( Blowfish_Decrypt, &_blowfish_context, buf, buflen );
	len = ntohl( *((uint32_t*)buf) );
	memmove( buf, buf + sizeof(uint32_t), len );

	return len;
}


const char * crypto_error( void ) {
	return NULL;
}


#ifdef UNIT_TEST_CRYPTO

#include <time.h>

int main( int argc, char *argv[] ) {

	srand( time(NULL) );

	if( argc >= 3 ) {

		struct stat info;
		const char *fname
			= argv[1];
		const char *key
			= argv[2];
		const bool ENCRYPT
			= (argc < 4);
		uint32_t iv
			= ENCRYPT
			? (uint32_t)rand()
			: strtol(argv[3],NULL,0);

		if( ENCRYPT )
			fprintf( stderr, "Encrypting with IV 0x%08x\n", iv );

		if( crypto_setkey( (const uint8_t*)key, strlen(key) ) )
			errx( -1, "crypto_setkey( %s, %ld )", key, strlen(key) );

		if( stat( fname, &info ) == 0 ) {
			FILE *fp;

			if( ENCRYPT ) {

				const uint32_t LEN
					= crypto_sizeof_ciphertext( info.st_size );
				uint8_t *buf
					= calloc( LEN, sizeof(uint8_t) );
				if( buf ) {
					if( (fp = fopen( fname, "r" )) ) {
						if( crypto_encrypt( fp, info.st_size, (uint8_t *)&iv, sizeof(iv), buf, LEN ) ) {
							err( -1, "crypto_encrypt" );
						}
#ifdef _DEBUG
						_report_bias( buf, LEN, stderr );
#endif
						fclose( fp );
						fwrite( buf, sizeof(uint8_t), LEN, stdout );
					} else
						err( -1, "fopen( %s, \"r\")", fname );
					free( buf );
				} else
					err( -1, "calloc( %d, %ld )", LEN, sizeof(uint8_t) );

			} else {

				int LEN
					= info.st_size;
				uint8_t *buf
					= calloc( LEN, sizeof(uint8_t) );
				if( buf ) {
					//uint32_t PLEN;
					if( (fp = fopen( fname, "r" )) ) {
						if( fread( buf, sizeof(uint8_t), LEN, fp ) != LEN )
							err( -1, "reading %s", fname );
						fclose( fp );
						LEN = crypto_decrypt( (uint8_t *)&iv, sizeof(iv), buf, LEN );
						if( LEN < 0 ) {
							errx( -1, "crypto_decrypt" );
						}
					}
					fwrite( buf, sizeof(uint8_t), LEN, stdout );
					free( buf );
				} else
					err( -1, "opening %s", fname );
			}

		} else
			err( -1, "stat( %s, ...)", fname );

	} else {
		
		fprintf( stderr, "%s <filename> <password> [ <iv> ]\n", argv[0] );
		return EXIT_FAILURE;	
	}

	return EXIT_SUCCESS;	
}
#endif

