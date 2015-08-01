
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

#ifndef _crypto_h_
#define _crypto_h_

/**
  * Set the key to be used for all subsequent encryption/decryption
  * operations.
  */
int crypto_setkey( const uint8_t *key, int len );

/**
  * Indicates how large the buffer must be to encrypt a payload of the given
  * size, accounting for both alignment requirements and header.
  */
int crypto_sizeof_ciphertext( int payload );

/**
  * Encrypt the <plaintextlen> bytes from the given open file (from the
  * current position of the file pointer), using the given initialization
  * vector <iv>. Leave the results in <buf>.
  */
int crypto_encrypt( FILE *fp, int plaintextlen, uint8_t *iv, int ivlen, uint8_t *buf, int buflen );

/**
  * Decrypt the contents of buf in place, leaving the result aligned with
  * the start of the buffer. (The header, if present, is overwritten.)
  */
int crypto_decrypt( uint8_t *iv, int ivlen, uint8_t *buf, int buflen );

/**/
const char *crypto_error( void );

#endif

