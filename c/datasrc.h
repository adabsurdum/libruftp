
/**
  * Library implementation of client and server side "RUFTP" protocol.
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

#ifndef _datasrc_h_
#define _datasrc_h_

/**
  * Request interpreter returns pointers to request structs which define
  * the packetization of the response.
  *
  * Server sets a timer when a request is first made and resets it each time
  * a resend is received for one of its packets.
  *
  * Server does not know anything about content of responses beyond that
  * they consist of N ordered and pre-defined packets any of which are
  * obtainable from the content provider.
  */
struct data {

	/**
	  * Returns the size in bytes of whatever was produce by _get.
	  */
	size_t (*size)( struct data * );

	/**
	  * This method returns the amount of buf actually used.
	  */
	uint16_t (*write_fragment)( struct data *, size_t off, size_t len, void *buf );

	void (*free)( struct data * );
};


/**
  * Implementors of this interface interpret incoming requests,ZZ
  */
struct data_source {

	/**
	  * Returns a packetized object.
	  * <len> specifies how many bytes in <buf> require provider's
	  * parsing/interpretation.
	  */
	struct data *(*get)( const void *buf, size_t len );

	/**
	  * Provides for client control of server (server's product).
	  */
	int (*control)( void *buf, size_t len );
};

#endif

