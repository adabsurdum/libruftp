
#ifndef _socket_h_
#define _socket_h_

struct sockaddr_in;

int udp_send( const uint8_t *buf, int buflen, int s, struct sockaddr_in *server );

#endif
