
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>    // for inet_pton
#include <netinet/in.h>
#include <errno.h>
#include <assert.h>
#include <err.h>

#include "socket.h"

int udp_send( const uint8_t *pkt, int pktlen, int s, struct sockaddr_in *server ) {

	static const char *ERR_REPORT
		= "\"%s\" in %s near (%s:%d)";
	int n = sendto( s, pkt, pktlen,
		0, (const struct sockaddr *)server, sizeof(struct sockaddr_in) );
	if( n < 0 ) {
		warn( ERR_REPORT, "sendto", __func__, __FILE__, __LINE__ );
#if 0
		switch( errno ) {
		case EACCES:	// (For UDP sockets) An attempt was made to send to a network/broadcast address as though it was a unicast address.
		case EAGAIN:// EWOULDBLOCK:
		case EBADF:		// An invalid descriptor was specified.
		case ECONNRESET: // Connection reset by peer.
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
				;
		}
#endif
	}
	return n == pktlen ? 0 : -1;
}

#ifdef UNIT_TEST_SOCKET
static const char *_ERR_REPORT
	= "\"%s\" in %s near (%s:%d)";

int udp_init_socket( short port, const char *if_addr ) {

	int s = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );

	if( s >= 0 ) {

		struct sockaddr_in addr;

		addr.sin_family      = AF_INET;
		addr.sin_port        = htons(port);
		if( if_addr ) {
			assert( sizeof(addr.sin_addr.s_addr) == 4 );
			inet_pton( AF_INET, if_addr, & addr.sin_addr.s_addr );
		} else
			addr.sin_addr.s_addr = INADDR_ANY;

		if( bind( s, (struct sockaddr *)&addr, sizeof(struct sockaddr))) {
			close( s );

			warn( _ERR_REPORT, "bind", __func__, __FILE__, __LINE__ );
			switch( errno ) {
			case EACCES:	// The address is protected, and the user is not the superuser.
			case EADDRINUSE:// The given address is already in use.
			case EBADF:		// sockfd is not a valid descriptor.
			case EINVAL:	// The socket is already bound to an address.
			case ENOTSOCK:	// sockfd is a descriptor for a file, not a socket.
			default:
				;
			}
		}

	} else {

		warn( _ERR_REPORT, "socket", __func__, __FILE__, __LINE__ );
		switch( errno ) {
		case EACCES:	// Permission to create a socket of the specified type and/or protocol is denied.
		case EAFNOSUPPORT:// The implementation does not support the specified address family.
		case EINVAL:	// Protocol unknown or unavailable or invalid flags.
		case EMFILE:	// Process file table overflow.
		case ENFILE:	// The system limit on the total number of open files has been reached.
		case ENOBUFS:
		case ENOMEM:	// Insufficient memory is available.  The socket cannot be created until sufficient resources are freed.
		case EPROTONOSUPPORT:// The protocol type or the specified protocol is not supported within this domain.
		default:
			;
		}
	}
	return s;
}


int main( int argc, char *argv[] ) {
	const short PORT
		= argc > 1 ? atoi(argv[1]) : 1234;
	int s = udp_init_socket( PORT, NULL );
	if( s >= 0 ) {
		close( s );
	} else
		warn( "udp_init_socket( %d )", PORT );
	return EXIT_SUCCESS;
}
#endif

