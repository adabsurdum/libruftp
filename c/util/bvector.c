
#include <stdlib.h>
#include <stdint.h>
#ifdef _DEBUG
#include <stdio.h>
#endif
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <err.h>

#include "boolvect.h"
#include "bvector.h"

typedef uint32_t word_t;

#define BITS_PER_WORD (sizeof(word_t)*8)

static inline int words( int bits ) {
	return ( bits/BITS_PER_WORD )
		+ (( bits%BITS_PER_WORD )?1:0);
}

struct bv {

	struct boolean_vector interface;

	/**
	  * number of flags
	  */
	int length;

	/**
	  * Number of word_t holding the flags.
	  */
	int word_count;

	/**
	  * The number of bits known to be set.
	  * An optimization to preclude useless iteration.)
	  */
	int popcount;

	/**
	  * The first (0-based) word to check for a set flag.
	  */
	int iter_word;

	word_t flags[0];
};


static int _cap( struct boolean_vector *pi ) {
	struct bv *po
		= (struct bv *)pi;
	return po->length;
}


static int _popcount( struct boolean_vector *pi ) {
	struct bv *po
		= (struct bv *)pi;
	return po->popcount;
}


static bool _setstate( struct boolean_vector *pi, int vnum, bool newstate ) {

	struct bv *po
		= (struct bv *)pi;

	assert( vnum < po->length );

	if( vnum < po->length ) {

		const int w
			= vnum / BITS_PER_WORD;
		const int bnum
			= vnum % BITS_PER_WORD;
		const int bval
			= (1<<bnum);
		const bool oldstate
			= ( po->flags[ w ] & bval ) != 0;

		// Nothing happens unless the old and new states are different.

		if( oldstate != newstate ) {
			po->flags[ w ] ^= bval; // toggle the bit
			po->popcount += ( newstate ? 1 : -1 );
		}

		assert( 0 <= po->popcount && po->popcount <= po->length );
		return oldstate;
	}

	err( -1, "invalid access %s:%d", __FILE__, __LINE__ );
	return false;
}


static bool _set( struct boolean_vector *pi, int vnum ) {
	return _setstate( pi, vnum, true );
}


static bool _clear( struct boolean_vector *pi, int vnum ) {
	return _setstate( pi, vnum, false );
}


/**
  *
  */
static int _clearnext( struct boolean_vector *pi ) {

	struct bv *po
		= (struct bv *)pi;
	int flagnum = -1; // ...meaning nothing is set.

	if( po->popcount > 0 /* Something somewhere is set. */ ) {

		int bit;

		// Find 1st word (which may be the "current" word) with a flag set.

#ifdef _DEBUG
		const int START
			= po->iter_word;
#endif
		// po->popcount > 0 implies the following loop must terminate.

		while( po->flags[ po->iter_word ] == 0 ) {

			po->iter_word = ( po->iter_word + 1 ) % po->word_count;
#ifdef _DEBUG
			if( po->iter_word == START ) {
				fprintf( stderr, "infinite loop: %s(%s:%d)\n",
					__func__, __FILE__, __LINE__ );
				abort();
			}
#endif
		}

		assert( po->flags[ po->iter_word ] != 0 );

		bit = __builtin_ctz( po->flags[ po->iter_word ] );
		flagnum = po->iter_word * BITS_PER_WORD + bit;

		assert( 0 <= flagnum && flagnum < po->length );

		// Clear the bit just read, and adjust the popcount accordingly.

		po->flags[ po->iter_word ] ^= (1<<bit);
		po->popcount -= 1;
	}
	return flagnum;
}


static int _write( struct boolean_vector *pi, uint32_t *arr, int n ) {
	struct bv *po
		= (struct bv *)pi;

	int written = 0;
	for(int i = 0; i < po->length && written < n; i++ ) {
		if( po->flags[ i / BITS_PER_WORD ] & ( 1 << (i % BITS_PER_WORD) ) )
			arr[ written++ ] = (uint32_t)i;
	}
	return written;
}

static void _destroy( struct boolean_vector *pi ) {
	struct bv *po
		= (struct bv *)pi;
	if( po )
		free( po );
}


struct boolean_vector *bv_create( int len, bool init_set ) {
	const int NWORDS
		= words( len );
	const size_t SIZEOF_OBJECT
		= sizeof(struct bv) + NWORDS*sizeof(word_t);
	struct bv *obj
		= malloc( SIZEOF_OBJECT );
	if( obj ) {

		memset( obj, 0, SIZEOF_OBJECT );

		obj->length     = len;
		obj->word_count = NWORDS;

		obj->interface.cap      = _cap;
		obj->interface.popcount = _popcount;
		obj->interface.setstate = _setstate;
		obj->interface.set      = _set;
		obj->interface.clr      = _clear;
		obj->interface.pop      = _clearnext;
		obj->interface.write    = _write;
		obj->interface.destroy  = _destroy;

		// All flags are now 0 (unset).

		if( init_set ) {
#if 0
			// WARNING: Must never allow unused bit in final word
			// to be set; they would foul up _clearnext
			memset( obj->flags, 0xFF, NWORDS*sizeof(word_t));
			obj->popcount = len;
#else
			for(int i = 0; i < len; i++ )
				obj->interface.set( & obj->interface, i );
#endif
		}

		return & obj->interface;
	}
	return NULL;
}


#ifdef UNIT_TEST_BVECTOR

#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
//#include <time.h>
#include <ctype.h>
#include <err.h>

int main( int argc, char *argv[] ) {
	//srand(time(NULL));
	if( argc > 2 ) {
		const int LEN
			= atoi(argv[1]);
		const int INI
			= atoi(argv[2]);
		struct boolean_vector *v
			= bv_create( LEN, INI );
		if( v ) {
			int n;
			bool state = false, _3arg = false;
			/**
			  * Every argument must be a /([CcSs]|[0-9]+)/
			  * Perform a command-line-specified series of operations
			  * on the boolean_vector, then...
			  */
			for(int i = 3; i < argc; i++ ) {
				const char *arg = argv[i];
				if( isdigit( arg[0] ) ) {
					const int flag = atoi(arg);
					if( _3arg ) {
						v->setstate( v, flag, state );
					} else {
						if( state )
							v->set( v, flag );
						else
							v->clr( v, flag );
					}
				} else {
					switch( arg[0] ) {
					case 's':
						state = true;
						_3arg = false;
						break;
					case 'c':
						state = false;
						_3arg = false;
						break;
					case 'S':
						state = true;
						_3arg = true;
						break;
					case 'C':
						state = false;
						_3arg = true;
						break;
					default:
						abort();
					}
				}
			}

			/**
			  * ...dump the results by .write as well as .pop.
			  */

			n = v->popcount(v);

			if( n > 0 ) {
				uint32_t *arr;
				printf( "Set bits (accessed by .write):\n" );
				arr = alloca( n * sizeof(uint32_t));
				v->write(v, arr, n );
				printf( "%d", arr[0] );
				for(int i = 1; i < n; i++ )
					printf( ",%d", arr[i] );

				printf( "\nSet bits (accessed by .pop):\n" );
				printf( "%d", v->pop(v) );
				while( (n = v->pop(v)) >= 0 )
					printf( ",%d", n );
				putchar( '\n' );
			} else
				puts( "no bits set" );

			assert( v->popcount(v) == 0 ); // exhausted by .pop above.

			v->destroy( v );
		}
	} else
		errx( -1, "%s <len> <init_set> bit1 [ bit2 bit3 ... ]\n", argv[0] );
	return EXIT_SUCCESS;
}
#endif

