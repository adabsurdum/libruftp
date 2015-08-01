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
#include <stdint.h>
#include <string.h>
#include <ctype.h>

/**
  * Prints binary data formatted like 'hexdump -C'. It has three
  * major column groups:
  * 1. address (offset, actually, from start of data)
  * 2. 16 bytes of data as 1-byte hexadecimal values
  * 3. the ASCII character corresponding to each byte when applicable
  *    (and '.' placeholders otherwise)
  *
  * ...for example...
  *
  * 35ACD3F0 30 31 32 33 34 35 36 37  6A 6B 6C 6D 6E 6F 70 71  01234567 jklmnopq
  *
  * Output will always be 76 characters wide
  * 8x1  8 offset
  * 3x8 24 binary data
  * 1x1  1 extra space
  * 3x8 24 binary data
  * 2x1  2 double space
  * 8x1  8 ASCII
  * 1x1  1 extra space
  * 8x1  8 ASCII
 */
void hexdump( const uint8_t *txt, int len, FILE *fp ) {

	static const char *HEX = "0123456789ABCDEF";

//	000000000011111111112222222222333333333344444444445555555555666666666677
//	012345678901234567890123456789012345678901234567890123456789012345678901
//	1234 00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f  01234567 89abcdef
	char buf[76];

	int c, i = 0;

	// Can't use the printf variants below unless we print sequentially 
	// since it inserts NULs.

	do {

		memset( buf,      ' ', 76 );
		sprintf( buf, "%08X", i ); /* remove the NUL */ buf[4] = ' ';

		for(c = 0; c < 8; c++ ) {
			if( i < len ) {
				const int v = txt[i++];
				buf[  5 + 3*c + 1] = HEX[ (v>>0)&0xF ];
				buf[  5 + 3*c    ] = HEX[ (v>>4)&0xF ];
				buf[ 55 +   c    ] = isprint(v) ? v : '.';
			} else {
				buf[  5 + 3*c + 1] = ' ';
				buf[  5 + 3*c    ] = ' ';
				buf[ 55 +   c    ] = ' ';
			}
		}
		for(c = 0; c < 8; c++ ) {
			if( i < len ) {
				const int v = txt[i++];
				buf[ 30 + 3*c + 1] = HEX[ (v>>0)&0xF ];
				buf[ 30 + 3*c    ] = HEX[ (v>>4)&0xF ];
				buf[ 64 +   c    ] = isprint(v) ? v : '.';
			} else {
				buf[ 30 + 3*c + 1] = ' ';
				buf[ 30 + 3*c    ] = ' ';
				buf[ 64 +   c    ] = ' ';
			}
		}
		// Find the last non-space character, and append "\n\0".
		c = 74; while( isspace(buf[c]) ) --c;
		strcpy( buf + c + 1, "\n" );
		fputs( buf, fp );
	} while( i < len );
}

