
#ifndef _server_h_
#define _server_h_

struct data_source;

void ruftp_server_loop( int socket_fd, struct data_source *provider, unsigned int max_silence_s );

#endif

