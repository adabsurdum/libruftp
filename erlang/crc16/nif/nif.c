
#include <stdint.h>
#include "erl_nif.h"

uint16_t crc16( const uint8_t *data, uint16_t size );

static ERL_NIF_TERM crc16_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	int ret;
	ErlNifBinary bin;
	if( ! enif_inspect_binary( env, argv[0], &bin ) ) {
		return enif_make_badarg( env );
	}
	ret = crc16( bin.data, bin.size );
	return enif_make_int( env, ret );
}

static ErlNifFunc nif_funcs[] = {
    {"crc16", 1, crc16_nif }
};

ERL_NIF_INIT( webcam_server, nif_funcs, NULL /*load*/, NULL /*reload*/, NULL /*upgrade*/, NULL /*unload*/ )

