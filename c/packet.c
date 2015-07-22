
/**
  * This module implements creation of client-to-server packets.
  * (Server only sends one type of packet, and its creation is inlined
  *  within the server itself.)
  */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <arpa/inet.h>
#include <assert.h>

#include "packet.h"

#include "util/crc16.h"
#include "util/boolvect.h"


/**
  * Build and send a GET packet to the server.
  */
int build_packet_get( uint32_t rid, uint16_t mtu, uint8_t *buf, int buflen ) {

	buf[OFFSET_PKT_TYPE]  = RUFTP_GET;
	buf[OFFSET_PKT_FLAGS] = 0;
	*(uint16_t*)(buf + OFFSET_PAYLOAD_SIZE) = htons( 0 );
	*( reqid_t*)(buf + OFFSET_RID)          = htonl( rid );
	*(uint16_t*)(buf + OFFSET_GET_MTU)      = htons( mtu );
	*(uint16_t*)(buf + OFFSET_GET_PAYLOAD)
		= htons( crc16( buf, OFFSET_GET_PAYLOAD /* b/c req is empty */ ) );

	return SIZEOF_GET_FF; /* because (currently) no payload */
}


int build_packet_res( uint32_t rid, int max, struct boolean_vector *missing, uint8_t *buf, int buflen ) {

	const int REMAINING
		= missing->popcount( missing );
	const int N 
		= REMAINING < max ? REMAINING : max;
	fragid_t *fid
		= (uint32_t *)(buf + OFFSET_RES_PAYLOAD);
	const size_t SIZEOF_PACKET_BODY
		= OFFSET_RES_PAYLOAD + N * sizeof(fragid_t);

	buf[OFFSET_PKT_TYPE]  = RUFTP_RES;
	buf[OFFSET_PKT_FLAGS] = 0;
	*(uint16_t*)( buf + OFFSET_PAYLOAD_SIZE) = htons( N * sizeof(fragid_t) );
	*( reqid_t*)( buf + OFFSET_RES_RID     ) = htonl( rid );

	/**
	  * Populate the payload with N fragids.
	  */

	for(int i = 0; i < N; i++ ) {
		const int FID
			= missing->pop( missing );
		assert( 0 <= FID );
		fid[i] = htonl( FID );
	}

	*(uint16_t*)(buf + SIZEOF_PACKET_BODY)
		= htons( crc16( buf, SIZEOF_PACKET_BODY ) );

	return SIZEOF_PACKET_BODY + SIZEOF_CRC16;
}


int build_packet_ack( uint32_t rid, uint8_t *buf, int buflen ) {

	buf[OFFSET_PKT_TYPE]  = RUFTP_ACK;
	buf[OFFSET_PKT_FLAGS] = 0;
	*( reqid_t*)(buf + OFFSET_ACK_RID)
		= htonl( rid );
	*(uint16_t*)(buf + OFFSET_ACK_CRC)
		= htons( crc16( buf, OFFSET_ACK_CRC ) );
	return SIZEOF_ACK_FF;
}


#ifdef UNIT_TEST_PACKET
#include <stdio.h>
int main( int argc, char *argv[] ) {

	printf( " OFFSET_GET_RID = %ld\n", OFFSET_GET_RID );
	printf( " OFFSET_GET_MTU = %ld\n", OFFSET_GET_MTU );
	printf( " OFFSET_GET_PAYLOAD = %ld\n", OFFSET_GET_PAYLOAD );

	printf( "  SIZEOF_GET_FF = %ld\n", SIZEOF_GET_FF );


	printf( " OFFSET_DAT_RID = %ld\n", OFFSET_DAT_RID );
	printf( " OFFSET_DAT_FID = %ld\n", OFFSET_DAT_FID );
	printf( " OFFSET_DAT_SOO = %ld\n", OFFSET_DAT_SOO );
	printf( "OFFSET_DAT_PAYLOAD = %ld\n", OFFSET_DAT_PAYLOAD );

	printf( "  SIZEOF_DAT_FF = %ld\n", SIZEOF_DAT_FF );


	printf( " OFFSET_RES_RID = %ld\n", OFFSET_RES_RID );
	printf( "OFFSET_RES_PAYLOAD = %ld\n", OFFSET_RES_PAYLOAD );

	printf( "  SIZEOF_RES_FF = %ld\n", SIZEOF_RES_FF );


	printf( " OFFSET_ACK_RID = %ld\n", OFFSET_ACK_RID );
	printf( " OFFSET_ACK_CRC = %ld\n", OFFSET_ACK_CRC );

	printf( "  SIZEOF_ACK_FF = %ld\n", SIZEOF_ACK_FF );

	if( argc > 2 ) {
		const size_t MTU
			= atoi( argv[1] );
		const size_t LEN
			= atoi( argv[2] );
		size_t bpf
			= MAX_SIZEOF_DAT_PAYLOAD(MTU);
		const int nfrag
		   = min_packet_count( LEN, MTU );
		printf( "With an MTU of %ld, %ld bytes of data requires %d fragments,\neach carrying %ld data bytes\n",
			MTU, LEN, nfrag, bpf );
	}

	return EXIT_SUCCESS;
}
#endif

