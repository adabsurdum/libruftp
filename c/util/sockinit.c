
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

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>    // for inet_pton, inet_ntop
#include <netinet/in.h>
#include <errno.h>
#include <assert.h>
#ifdef _DEBUG
#include <err.h>
static const char *DBG_ERR_REPORT
	= "\"%s\" in %s near (%s:%d)";
#endif

#include "socket.h"

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
#ifdef _DEBUG
			warn( DBG_ERR_REPORT, "bind", __func__, __FILE__, __LINE__ );
#endif
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

#ifdef _DEBUG
		warn( DBG_ERR_REPORT, "socket", __func__, __FILE__, __LINE__ );
#endif
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


#ifdef UNIT_TEST_SOCKINIT
#include <err.h>
int main( int argc, char *argv[] ) {
	const short PORT
		= argc > 1 ? atoi(argv[1]) : 0;
	const char *ADDR
		= argc > 2 ? argv[2] : NULL;
	int s = udp_init_socket( PORT, ADDR );
	printf( "udp_init_socket( %d, %s ) => %d\n", PORT, ADDR ? ADDR : "NULL", s );
	assert( sizeof(struct sockaddr) == sizeof(struct sockaddr_in) );
	if( s >= 0 ) {
		struct sockaddr_in addr;
		socklen_t length = sizeof(addr);
		if( getsockname( s, (struct sockaddr*)(&addr), &length ) == 0 && sizeof(addr) == length ) {
			char buf[128];
			if( inet_ntop( AF_INET, &addr.sin_addr, buf, sizeof(buf) ) == buf ) {
				printf( "bound to %s:%d\n", buf, ntohs( addr.sin_port ) );
			} else
				warn( "inet_ntop" );
		} else
			warn( "getsockname" );
		close( s );
	} else
		warn( "udp_init_socket( %d, %s ) => %d\n", PORT, ADDR ? ADDR : "NULL", s );
	return EXIT_SUCCESS;
}
#endif

