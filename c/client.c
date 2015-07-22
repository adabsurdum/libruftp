
#if defined(DUMP_PACKETS) || defined(_DEBUG)
#include <stdio.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <err.h>

#include "client.h"
#include "packet.h"

#include "util/socket.h"
#include "util/crc16.h"
#include "util/boolvect.h"
#include "util/bvector.h"
#ifdef DUMP_PACKETS
#include "util/hexdump.h"
#endif

/**
  * This function fully implements a single "fetch" (GET) from a RUFTP server.
  */
ssize_t ruftp_fetch( int s, struct sockaddr_in *server, uint16_t mtu, int retries, uint8_t **data ) {

	static uint8_t iobuf[ 512 ];
	int PACKET_LENGTH;
	ssize_t result = -1;
	uint8_t *payload = iobuf + OFFSET_DAT_PAYLOAD; // ...always in same place.

	bool timed_out = false;
	size_t sizeof_object = 0;
	uint8_t *object_buffer = NULL;
	struct boolean_vector *missing = NULL;

	struct timeval timeout = {
		.tv_sec  = 3,
		.tv_usec = 0
	};
	socklen_t addrlen = sizeof(struct sockaddr_in);
	size_t /* DATA! */ sizeof_fragment;
	int fragment_count;

	ssize_t n = 10;

	/**
	  * Generate a random request id, and insure it's not zero.
	  */

	uint32_t RID = rand();
	while( RID == 0 && n-- > 0 /* to guarantee inf loop impossible */ )
		RID = rand();
	if( RID == 0 )
		err( -1, "unable to generate non-zero random request id" );

	//memset( &H, 0, sizeof(H));

	/**
	  * 1. Send the GET request and wait a fixed amount of time for an
	  *    initial DAT packet response.
	  *    Resend the GET request up to <retries> times in the event of
	  *    timeout(s).
	  *    The initial DAT packet is effectively an ACK--says the server
	  *    heard us--and establishes the parameters of the response.
	  */

	setsockopt( s, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout) );

	while( true ) {

		/**
		  * Send a command...
		  */

		int psz, rid, soo, fid, fret = 0;
		if( missing /* at least one DAT packet has been rcvd */ ) {
			if( timed_out ) {
				PACKET_LENGTH = build_packet_res(
						RID,
						( mtu - SIZEOF_RES_FF ) / sizeof(fragid_t),
						missing,
						iobuf,
						sizeof(iobuf) );
				if( udp_send( iobuf, PACKET_LENGTH, s, server ) )
					errx( -1, "foo!" );

			}
		} else
		if( --retries > 0 ) {
			PACKET_LENGTH = build_packet_get( RID, mtu, iobuf, sizeof(iobuf) );
			if( udp_send( iobuf, PACKET_LENGTH, s, server ) )
				errx( -1, "foo!" );
		} else
			return -1;

		if( fret ) {
			warnx( "failed sending %s", missing ? "RES" : "GET" );
			break;
		}

		/**
		  * ...and await a response (which can only be DAT packets).
		  */

		memset( iobuf, 0, sizeof(iobuf) );

		addrlen = sizeof(struct sockaddr_in);
		n = recvfrom( s, iobuf, sizeof(iobuf),
			0, (struct sockaddr *)server, &addrlen );

		if( n < 0 ) {

			if( EAGAIN == errno || EWOULDBLOCK == errno ) {
				warnx( "Sent %s %ld.%06lds ago with no response. Resending.",
					missing ? "RES" : "GET",
					timeout.tv_sec, timeout.tv_usec );
				timed_out = true;
				continue;
			}

			// Any other error is game-over...
			
			warn( "recvfrom: %s (%s:%d)", __func__, __FILE__, __LINE__ );
			break; // out of loop
#if 0
			switch( errno ) {
			case EINTR:// The receive was interrupted by delivery of a signal before any data were available; see signal(7).
			case EBADF:  // The argument sockfd is an invalid descriptor.
			case ECONNREFUSED:// A remote host refused to allow the network connection (typically because it is not running the requested service).
			case EFAULT:// The receive buffer pointer(s) point outside the process's address space.
			case EINVAL:// Invalid argument passed.
			case ENOMEM:// Could not allocate memory for recvmsg().
			case ENOTCONN:// The socket is associated with a connection-oriented protocol and has not been connected (see connect(2) and accept(2)).
			case ENOTSOCK:
			}
#endif
		}

		assert( n > 0 ); // If we reach here, we have a non-empty packet.

		timed_out = false; // Reset on each reception.
		//attempts = 0; 

#ifdef DUMP_PACKETS
		fprintf( stderr, "Received packet(%ld bytes):\n", n );
		hexdump( iobuf, 9, stderr );
#endif

		/**
		  * Current server only sends one packet type to client:
		  */

		if( RUFTP_DAT != iobuf[0] ) {
			warnx( "unexpected packet type %02X", iobuf[0] );
			break;
		}

		/**
		  * Verify the packet's integrity.
		  */

		if( crc16( iobuf, n-2 ) != ntohs( *(uint16_t*)(iobuf + n - 2) ) ) {
			warnx( "invalid checksum" );
			abort();
		}

		psz = ntohs( *(uint16_t*)(iobuf + OFFSET_PAYLOAD_SIZE) );
		rid = ntohl( *( reqid_t*)(iobuf + OFFSET_DAT_RID) );
		soo = ntohl( *(uint32_t*)(iobuf + OFFSET_DAT_SOO) );
		fid = ntohl( *(fragid_t*)(iobuf + OFFSET_DAT_FID) );

		if( rid != RID ) {
			warnx( "current request %08x, received (%08x)", RID, rid );
		}

		if( ! missing /* ...then this is the 1st DAT packet. */ ) {

			/**
			  * Allocate a buffer for the object, and set up the bitvector
			  * that records which fragments we have received.
			  * Note that the 10 bytes of (packet_size,rid,object_size)
			  * should be identical in every received DAT packet.
			  */

			sizeof_object = soo;
			object_buffer = malloc( sizeof_object );
			if( object_buffer == NULL )
				err( -1, "allocating frame buffer" );
			memset( object_buffer, 0, sizeof_object );

			/**
			  * Calculate the packetization.
			  */

			if( (iobuf[1] & FLAG_TAIL_FRAG) != 0 ) {

				fragment_count
					= fid+1;
				sizeof_fragment
					= (sizeof_object-psz) / fragment_count;

				assert( ((sizeof_object-psz) % fragment_count) == 0 );
				assert( sizeof_fragment*fid + psz == sizeof_object );

			} else {
				sizeof_fragment
					= psz;
				fragment_count
					= sizeof_object / sizeof_fragment
					+ ( (sizeof_object % sizeof_fragment) > 0 ? 1 : 0);

				assert( (fragment_count-1)*sizeof_fragment <= sizeof_object );
				assert( sizeof_object <=  fragment_count*sizeof_fragment );
			}

#ifdef _DEBUG
			fprintf( stderr, "fragment-count = %d\nbytes-per-fragment = %ld\n",
				fragment_count, sizeof_fragment );
#endif
			/**
			  * Allocate boolvect to monitor missing blocks.
			  */

			missing = bv_create( fragment_count, 1 /* all are initially set */ );
		}

		assert( 0 <= fid && fid < fragment_count );

		/**
		  * Copy the pixel data to its intended destination in the frame,
		  * and mark it "present".
		  */

		assert( fid*sizeof_fragment < sizeof_object );

		/**
		  * Copy pixel data verbatim into frame buffer. TODO: May need
		  * to do some ntoh or other transformations...
		  */

		memcpy(
			object_buffer + fid*sizeof_fragment,
			payload,
			psz );

		missing->clr( missing, fid );

		/**
		  * Is the shipment complete? Bail out, if so...
		  */

		if( missing->popcount( missing ) == 0 ) {
			result = sizeof_object;
			break;
		}
	}

	PACKET_LENGTH = build_packet_ack( RID, iobuf, sizeof(iobuf) );
	if( udp_send( iobuf, PACKET_LENGTH, s, server ) )
		errx( -1, "foo!" );

	if( data )
		*data = object_buffer;
	return result;
}

