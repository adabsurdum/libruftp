
/**
  * Queue makes sense for transmission if multiple requests are to be
  * handled simultaneously. Otherwise, boolean "pending" vectors that are
  * scanned linearly make more sense. If a RESEND request comes in for a
  * packet already pending (e.g. because client got impatient), setting the
  * flag is idempotent, so no unnecessary/redundant transmission will occur.
  * Linearly scanning is only inefficient when the vector is sparse, and
  * maintenance of a count of set bits precludes futile scanning.
  *
  * TFTP only allows one file "in flight" at a time which seems
  * reasonable for this because I am trying to support
  * 1) strictly peer to peer and
  * 2) low-volume transfers
  * Doesn't have to be P2P. One family may try to access a camera from
  * two phones, and it may be simultaneous. Reasonable to return BUSY.
  */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#ifdef LOG_INCOMING
#include <arpa/inet.h>
#endif
#include <time.h>
#include <netdb.h>
#include <errno.h>
#include <err.h>

#include "packet.h"
#include "server.h"
#include "datasrc.h"

#include "util/boolvect.h"
#include "util/bvector.h"
#include "util/crc16.h"
#ifdef DUMP_PACKETS
#include "util/hexdump.h"
#endif

static const char *ERR_REPORT
	= "\"%s\" in %s near (%s:%d)";


/**
  * BEGIN STATE
  * The following six variables are created and destroyed around each RUFTP
  * request. The data source is responsible for packetizing whatever it
  * determines was requested (in data_source.get).
  * This protocol really straddles session and presentation, using UDP as
  * the transport.
  */
static struct data *_data   = NULL;

/**
  * These are determined by the last GET request.
  */
static size_t _sizeof_data      = 0;
static size_t _sizeof_fragment  = 0;
static size_t _sizeof_remainder = 0;
static int _fragment_count      = 0;

static struct boolean_vector *_pending = NULL;

static struct sockaddr_in _req_addr;
static int _mtu     = 0;
static reqid_t _rid = 0;

/**
  * END STATE
  */

static inline bool _is_final_frag( int f ) {
	return (f+1) == _fragment_count;
}

static void _clear_request() {

	_data->free( _data );
	_data = NULL;

	_pending->destroy( _pending );
	_pending = NULL;

	_sizeof_data      = 0;
	_sizeof_fragment  = 0;
	_sizeof_remainder = 0;
	_fragment_count   = 0;

	memset( &_req_addr, 0, sizeof(_req_addr) );
	_mtu = 0;
	_rid = 0;
}


static void _handle_incoming( const uint8_t *buf, int n, struct data_source *provider ) {

	/**
	  * All packet types first 8 bytes are identical.
	  */

	const uint8_t PACKET_TYPE
		= buf[0];
	//const uint8_t FLAGS
	//	= buf[1];
	const uint16_t PAYLOAD_SIZE
		= ntohs( *((uint16_t*)(buf + OFFSET_PAYLOAD_SIZE)) );
	const reqid_t RID
		= ntohl( *((reqid_t*)(buf + OFFSET_RID)) );

	/**
	  * All packet types last 2 bytes are the CRC.
	  */

	if( ntohs( *(uint16_t*)(buf + n-2) ) != crc16( buf, n-2 ) ) {
		warnx( "invalid checksum" );
		abort();
	}

	if( PACKET_TYPE & RUFTP_BIT /* it's a RUFTP packet */ ) {

		switch( PACKET_TYPE ) {

		case RUFTP_GET:
			if( _rid == 0
					|| _rid == RID /* reGET as opposed to reSEND */ ) {
				if( _rid == 0 /* it's new */ ) {
					_mtu = ntohs( *((uint16_t*)(buf + OFFSET_GET_MTU)) );
					_sizeof_fragment = MAX_SIZEOF_DAT_PAYLOAD(_mtu);
					_rid = RID;
					_data = provider->get(
						buf + OFFSET_GET_PAYLOAD,// skip PKT_ID(1) and MTU(2)
						PAYLOAD_SIZE );
					if( _data ) {
						_sizeof_data = _data->size( _data );
						_fragment_count
							= min_packet_count( _sizeof_data, _mtu );
						_sizeof_remainder
							= ( _sizeof_data % _sizeof_fragment )
							? ( _sizeof_data % _sizeof_fragment )
							: _sizeof_fragment;
						_pending = bv_create( _fragment_count, 1 );
					} else {

						// Whatever was sent, wasn't received, so...
						// _pending->clear( _pending ); TODO
					}
#ifdef _DEBUG
					// TODO: Turn this into proper logging.
					printf( "_fragment_count = %d, _sizeof_fragment = %ld\n",
						_fragment_count, _sizeof_fragment );
#endif
				} else
					warnx( "failed getting data for %d-byte request (TODO) hex", PAYLOAD_SIZE );
			} else {
				// _rid is valid, so a request is in progress, but it's not
				// the same as the new one. TODO: Drop it on the ground or
				// optionally send client an error packet?
			}
			break;

		case RUFTP_RES:
			if( _rid == RID ) {
				const int N
					= PAYLOAD_SIZE
					/ sizeof(fragid_t);
				if( N > 0 ) {
					const fragid_t *frag
						= (const fragid_t *)(buf + OFFSET_RES_PAYLOAD);
					for(int i = 0; i < N; i++ ) {
						const int f = ntohl( frag[i] );
						_pending->set( _pending, f );
					}
				}
			} else {
				// TODO: log it properly and OPTIONALLY send client error msg.
				warnx( "request id does not match that in progress (%d)", __LINE__ );
			}
			break;

		case RUFTP_ACK:
			if( _rid == RID ) {
				_clear_request();
			} else {
				// TODO: log it properly and OPTIONALLY send client error msg.
				warnx( "request id does not match that in progress (%d)", __LINE__ );
			}
			break;
		}

	} else { // it's a application packet
		// TODO
	}
}


/**
  * This function implements an RUFTP server independent of the type of
  * content being served.
  */
void ruftp_server_loop( int socket_fd, struct data_source *provider, unsigned int max_silence_s ) {

#ifdef LOG_INCOMING
	static const char *TIME_FORMAT
		= "%y-%m-%d_%I:%M:%S%p";
	// "2013-11-11  6:06:24PM" is 21 characters
	static char sender[ INET_ADDRSTRLEN ];
#endif
	static uint8_t iobuf[ 512 ]; // ...enough for all expected packets.

	time_t time_to_abandon;
	struct sockaddr_in addr;
	socklen_t addrlen;
	int n;

#ifdef HAVE_SIM_DROPPED_PACKETS
	int dropped_packet_threshold = 0;
	if( getenv( "P_DROPPED_PACKET" ) ) {
		const double P
			= atof( getenv( "P_DROPPED_PACKET" ) );
		double thresh = RAND_MAX;
		thresh *= P;
		if( P < 0.0 || P > 1.0 ) 
			errx( -1, "P(dropped packet) (%f) must be in [0.0,1.0]", P );
		dropped_packet_threshold = (int)(thresh);
		fprintf( stderr,
			"simulating dropped packets with %.0f%% probability (%d)\n",
			P*100.0, dropped_packet_threshold );
	}
#endif

idle:
	memset( &_req_addr, 0, sizeof(addr) );

	/**
	  * Idle. Nothing to transmit (or even do) until a command arrives,
	  * so we can block indefinitely in recvfrom.
	  */

	addrlen = sizeof(addr);
	n = recvfrom( socket_fd, iobuf, sizeof(iobuf), 0, (struct sockaddr *)&addr, &addrlen );

	if( n < 0 ) {
		switch( errno ) {
		case EAGAIN:// EWOULDBLOCK:
		case EBADF:  // The argument sockfd is an invalid descriptor.
		case ECONNREFUSED:// A remote host refused to allow the network connection (typically because it is not running the requested service).
		case EFAULT:// The receive buffer pointer(s) point outside the process's address space.
		case EINTR:// The receive was interrupted by delivery of a signal before any data were available; see signal(7).
		case EINVAL:// Invalid argument passed.
		case ENOMEM:// Could not allocate memory for recvmsg().
		case ENOTCONN:// The socket is associated with a connection-oriented protocol and has not been connected (see connect(2) and accept(2)).
		case ENOTSOCK:
		default:
			warn( ERR_REPORT, "recvfrom", __func__, __FILE__, __LINE__ );
		}
		return;
	}

#ifdef LOG_INCOMING 
	{
		const time_t ctime = time(NULL);
		char tbuf[32];
		struct tm ltime;
		localtime_r( &ctime, &ltime );
		strftime( tbuf, 32, TIME_FORMAT, &ltime );
		inet_ntop( AF_INET, &addr.sin_addr, sender, sizeof(sender) );
		fprintf( stdout, "received packet from %s @ %s:\n", sender, tbuf );
#ifdef DUMP_PACKETS
		hexdump( iobuf, n, stdout );
#endif
	}
#endif

	// Until we return to "idle" above this is the only host from which
	// we'll accept packets.

	memcpy( &_req_addr, &addr, sizeof(addr) );
	_handle_incoming( iobuf, n, /*&addr, addrlen, */ provider );

	time_to_abandon = time(NULL) + max_silence_s;

	/**
	  * Send and receive.
	  */

	while( _data != NULL ) {

		int ret;
		fd_set rd_set;
		fd_set wr_set;

		struct timeval timeout = {
			.tv_sec  = 2,
			.tv_usec = 0
		};

		if( time(NULL) >= time_to_abandon ) {
			fprintf( stderr, "abandoning %08x\n", _rid );
			break;
		}

		FD_ZERO( &rd_set );
		FD_ZERO( &wr_set );
		FD_SET( socket_fd, &rd_set );
		FD_SET( socket_fd, &wr_set );

		/**
		  * Calculate timeout.
		  * If we need to await signals, use pselect, but note that select
		  * modifies timeout before returning to reflect remaining time.
		  * pselect does not.
		  * No need to wait on write availability unless we actually have
		  * something to send.
		  */
		ret = select( socket_fd + 1,
			&rd_set,
			_pending->popcount(_pending) > 0 ? &wr_set : NULL,
			NULL /* fd_set * exceptfds */,
			&timeout );
		if( ret < 0 ) {
			switch( errno ) {
			case EBADF:
			case EINTR:
			case EINVAL:
			case ENOMEM:
			default:
				warn( ERR_REPORT, "select", __func__, __FILE__, __LINE__ );
			}
			break;
		}

		// Transmit first...

		if( FD_ISSET( socket_fd, &wr_set) ) {
			const int frag
				= _pending->pop( _pending );

			if( frag >= 0 ) {
				const uint16_t SIZEOF_PAYLOAD
					=  _is_final_frag( frag )
					? _sizeof_remainder
					: _sizeof_fragment;
				const size_t SIZEOF_PACKET_BODY // header + payload - checksum
					= OFFSET_DAT_PAYLOAD + SIZEOF_PAYLOAD;

				// Let the data source provide the payload.
				_data->write_fragment( _data,
							frag * _sizeof_fragment,
							SIZEOF_PAYLOAD,
							iobuf + OFFSET_DAT_PAYLOAD );

				// Then build its header...
				iobuf[0] = RUFTP_DAT;
				iobuf[1] = _is_final_frag(frag) ? FLAG_TAIL_FRAG : 0;
				*(uint16_t*)( iobuf + OFFSET_PAYLOAD_SIZE) = htons( SIZEOF_PAYLOAD );
				*(reqid_t*)(  iobuf + OFFSET_DAT_RID) = htonl( _rid );
				*(uint32_t*)( iobuf + OFFSET_DAT_SOO) = htonl( _sizeof_data );
				*(fragid_t*)( iobuf + OFFSET_DAT_FID) = htonl( frag );

				// ...and append the checksum.
				*(uint16_t*)(iobuf + SIZEOF_PACKET_BODY)
					= htons( crc16( iobuf, SIZEOF_PACKET_BODY ) );

#ifdef HAVE_SIM_DROPPED_PACKETS
				if( rand() > dropped_packet_threshold ) {
#endif
#ifdef DUMP_PACKETS
					fprintf( stderr, "After sending %d, %d pending\n",
						frag, _pending->popcount(_pending) );
					hexdump( iobuf, OFFSET_DAT_PAYLOAD, stderr );
#endif
					n = sendto( socket_fd, iobuf, SIZEOF_PACKET_BODY+SIZEOF_CRC16, 
						0, (const struct sockaddr *)&_req_addr, sizeof(_req_addr) );
#ifdef HAVE_SIM_DROPPED_PACKETS
				} else {
					n = 0;
					fprintf( stderr, "dropping %d\n", frag );
				}
#endif
				if( n < 0 ) {
					switch( errno ) {
					case EACCES:	// (For UDP sockets) An attempt was made to send to a network/broadcast address as though it was a unicast address.
					case EAGAIN:    // == EWOULDBLOCK:
					case EBADF:		// An invalid descriptor was specified.
					case ECONNRESET:// Connection reset by peer.
					case EDESTADDRREQ: // The socket is not connection-mode, and no peer address is set.
					case EFAULT:	// An invalid user space address was specified for an argument.
					case EINTR:		// A signal occurred before any data was transmitted; see signal(7).
					case EINVAL:	// Invalid argument passed.
					case EISCONN:	// The  connection-mode socket was connected already but a recipient was specified.
					case EMSGSIZE:	// The socket type requires that message be sent atomically, and the size of the message to be sent made this impossible.
					case ENOBUFS:	// The output queue for a network interface was full. (Normally, this does not occur in Linux.  Packets are just silently dropped when a device queue overflows.)
					case ENOMEM:	// No memory available.
					case ENOTCONN:	// The socket is not connected, and no target has been given.
					case ENOTSOCK:	// The argument sockfd is not a socket.
					case EOPNOTSUPP:// Some bit in the flags argument is inappropriate for the socket type.
					case EPIPE:		// The local end has been shut down on a connection oriented socket.  In this case the process will also receive a SIGPIPE unless MSG_NOSIGNAL is set.
					default:
						warn( ERR_REPORT, "sendto", __func__, __FILE__, __LINE__ );
					}
					break;
				}
			}
		}

		// ...but remain receptive to incoming packets, too.

		if( FD_ISSET( socket_fd, &rd_set ) ) {
			addrlen = sizeof(addr);
			n = recvfrom(socket_fd, iobuf, sizeof(iobuf), 0, (struct sockaddr *)&addr, &addrlen);
			if( n < 0 ) {
				switch( errno ) {
				default:
					warn( ERR_REPORT, "recvfrom", __func__, __FILE__, __LINE__ );
				}
				return;
			}
#ifdef LOG_INCOMING 
			const time_t ctime = time(NULL);
			char tbuf[32];
			struct tm ltime;
			localtime_r( &ctime, &ltime );
			strftime( tbuf, 32, TIME_FORMAT, &ltime );
			inet_ntop( AF_INET, &addr.sin_addr, sender, sizeof(sender) );
			fprintf( stdout, "received packet from %s @ %s:\n", sender, tbuf );
#ifdef DUMP_PACKETS
			hexdump( iobuf, n, stdout );
#endif
#endif

			// Reset the waiting time.
			time_to_abandon = time(NULL) + max_silence_s;

			// TODO: This cmp may be too exhaustive.
			if( memcmp( &_req_addr, &addr, sizeof(addr) ) == 0 )
				_handle_incoming( iobuf, n, /*&addr, addrlen,*/ provider );
			else
				warnx( "dropping packet from 2nd host" );
		}
	}

	if( _data )
		_clear_request();

	goto idle;
}


#ifdef UNIT_TEST_SERVER

int main( int argc, char *argv[] ) {
	return EXIT_SUCCESS;
}

#endif

