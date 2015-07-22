
#ifndef _client_h_
#define _client_h_

struct sockaddr_in;

ssize_t ruftp_fetch( int s, struct sockaddr_in *server, uint16_t mtu, int retries, uint8_t **data );

#endif

