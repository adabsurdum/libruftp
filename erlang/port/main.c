
#include <stdlib.h>
#include <stdint.h>
#ifdef _DEBUG
#include <syslog.h>
#endif

#include "erl_interface.h"
#include "ei.h"
#include "rw.h"

extern const byte TEST_IMAGE[];
extern size_t SIZEOF_IMAGE;

#define WIRE_SIZEOF_INT (5)

static inline int min_packet_count( size_t sizeof_object, size_t sizeof_fragment ) {
	return (sizeof_object / sizeof_fragment)
		+ ((sizeof_object % sizeof_fragment) > 0 ? 1 : 0);
}

/**
  * Read an integer giving the maximum size of a fragment.
  */
int main( int argc, char *argv[] ) {

	int fail = 0;

	erl_init(NULL,0);
#ifdef _DEBUG
	openlog( "erlang-port", 0, LOG_USER );
	syslog( LOG_INFO, "%s is alive\n", argv[0] );
#endif
	while( ! fail ) {

		ETERM *fragsize_term = NULL;
		byte buf[100 /*WIRE_SIZEOF_INT*/];
		if( read_lpm( buf, sizeof(buf) ) <= 0 ) {
#ifdef _DEBUG
			syslog( LOG_ERR, "failed reading fragment size:\n" );
#endif
			break;
		}

		fragsize_term = erl_decode( buf );

		if( fragsize_term ) {

			ETERM **array;
			unsigned int count;
			unsigned int SIZEOF_FRAGMENT
				= ERL_INT_UVALUE(fragsize_term);
			erl_free_term( fragsize_term );

			if( SIZEOF_FRAGMENT == 0 /* the explicit signal to shutdown */ )
				break;

#ifdef _DEBUG
			syslog( LOG_INFO, "received sizeof fragment: %d\n", SIZEOF_FRAGMENT );
#endif

			count = min_packet_count( SIZEOF_IMAGE, SIZEOF_FRAGMENT );
			array = calloc( count, sizeof(ETERM*) );
			if( array ) {
				ETERM *frags;
				for(int i = 0; i < count; i++ ) {
					const size_t S
						= i+1 < count
						? SIZEOF_FRAGMENT
						: (SIZEOF_IMAGE % SIZEOF_FRAGMENT);
					array[i] = erl_mk_binary(
						TEST_IMAGE + i*SIZEOF_FRAGMENT, S );
					if( array[i] == NULL ) {
						count = i; // ...for erl_free_array below.
						fail = 1;
						break;
					}
				}
				if( fail )
					goto cleanup;
				frags = erl_mk_list( array, count );
				if( frags ) {
					const int nbyte = erl_term_len( frags );
					byte *buf = calloc( nbyte, sizeof(byte) );
					if( buf ) {
						if( erl_encode( frags, buf ) == nbyte ) {
							if( write_lpm( buf, nbyte ) != nbyte )
								fail = 1;
						} else
							fail = 1;
						free( buf );
					} else
						fail = 1;
				} else
					fail = 1;
cleanup:
				erl_free_array( array, count );
				free( array );
			}
		} else
			break;
	}
#ifdef _DEBUG
	closelog();
#endif
	return 0;
}

