
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

