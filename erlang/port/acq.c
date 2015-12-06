
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

/**
  * 1. Image is acquired either from a video device (or "snow" is 
  *    generated) for device-less testing.
  * 2. The image is converted from video format to direct color space (DCS)
  *    format.
  * 3. The DCS representation is then converted to a PNG file in a tmpfile.
  * 4. The PNG file is loaded
  * 5. The PNG is encrypted and becomes the "object" that is delivered by
  *    the protocol.
  */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/socket.h>
#include <time.h> // just for use as a random seed.
#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <err.h>
#ifdef _DEBUG
#include <zlib.h>
#endif
#include "png.h"

#include "packet.h"
#include "datasrc.h"
#include "socket.h"
#include "sockinit.h"
#include "crc16.h"
#include "server.h"

#include "yuyv.h"
#include "vidfmt.h"
#include "video.h"

static struct video_capture * _vci = NULL;

static struct video_format VIDEO_FORMATS[] = {
	{
		.width  = DEFAULT_VIDEO_WIDTH,
		.height = DEFAULT_VIDEO_HEIGHT,
		.pixel_format  = {'Y','U','Y','V','\0'}
	}
};


/**
  * These are determined by video format (currently fixed at startup).
  */
static size_t _sizeof_vid_pixel = 0;
static size_t _sizeof_vid_buffer = 0;
static uint8_t *_vid_buffer = NULL;

static int _samples_per_dcs_pixel = 3; // RGB

/**
  * This is used both for the DCS representation AND for the PNG
  * reloaded from its tmp file since the PNG form will always be
  * smaller than the DCS form, and we never need both simultaneously.
  */
static size_t _sizeof_obj_buffer = 0;
static uint8_t *_obj_buffer = NULL;

/**
  * _sizeof_obj <= _sizeof_obj_buffer
  */
static size_t _sizeof_obj = 0;

/**
  * Fragmented data.
  */

static size_t _fd_size( struct data *fd ) {
	return _sizeof_obj;
}


static uint16_t _fd_write_fragment( struct data *fd, size_t off, size_t len, void *payload ) {

	memcpy( payload, _obj_buffer + off, len );
	return (uint16_t)len;
}


static void _fd_free( struct data *fd ) {
	// no-op since pc points to a static object
#ifdef _DEBUG
	printf( "crc32(image) = %08X\n",
		(uint32_t)crc32( crc32(0, NULL, 0 ), _obj_buffer, _sizeof_obj ) );
#endif
}


struct data _interface = {
	.size           = _fd_size,
	.write_fragment = _fd_write_fragment,
	.free           = _fd_free
};

#ifdef HAVE_TIMESTAMP

#include "rastfont.h"

static void _overlay_time_rgb( const char *buf, uint8_t *topleft, size_t stride ) {
	while( *buf ) {
		const int c = *buf++;
		if( 0x21 <= c && c < 0x7F ) {
			const struct raster_char *px = charmap[c];
			for(int i = 0; i < px->count; i++ ) {
				const struct coord2d offset
					= px->foreground[i];
				uint8_t *pxc
					= topleft
					+ offset.y*stride
					+ offset.x*_samples_per_dcs_pixel;
				*pxc++ = 0xFF;
				*pxc++ = 0xFF;
				*pxc++ = 0xFF;
			}
		}
		topleft += (charcell_width*_samples_per_dcs_pixel); // charwidth
	}
}
#endif

/**
  */
static struct data *_get( const void *buf, size_t len ) {

	struct data *object_interface = & _interface;
	FILE *fp = NULL;
	long sizeof_png = 0;
	const int W = VIDEO_FORMATS[0].width;
	const int H = VIDEO_FORMATS[0].height;

#ifdef HAVE_TIMESTAMP
	static const char *TIME_FORMAT
		= "%y-%m-%d_%I:%M:%S%p";
	// "2013-11-11  6:06:24PM" is 21 characters
	char tbuf[32];
	const time_t ctime = time(NULL);
	struct tm ltime;
	localtime_r( &ctime, &ltime );
	if( strftime( tbuf, 32, TIME_FORMAT, &ltime ) == 0 )
		warnx( "strftime failed" );
#endif

	printf( "%s( %p, %ld )\n", __func__, buf, len );

	if( _vci->snap( _vci, &_sizeof_vid_buffer, (uint8_t**)&_vid_buffer ) ) {
		warnx( "failed snapshot" );
		return NULL;
	}

	yuyv2rgb( (const uint16_t *)_vid_buffer, W, H, _obj_buffer );

#ifdef HAVE_TIMESTAMP
	_overlay_time_rgb( tbuf,
		_obj_buffer + ((H-charcell_height-2)*W + 2)*_samples_per_dcs_pixel, // assuming 8x17
		W*_samples_per_dcs_pixel /* stride */);
#endif

	fp = tmpfile();
	if( NULL == fp ) {
		warn( "failed creating tmpfile" );
		return NULL;
	}

	png_write( fp, _obj_buffer, W, H, "", _samples_per_dcs_pixel );
	sizeof_png = ftell( fp );

	_sizeof_obj // may be > sizeof_png...
		= sizeof_png;

	// This should never be necessary, but never say never...

	if( _sizeof_obj > _sizeof_obj_buffer ) {
		warnx( "Encrypted PNG file (%ld bytes) larger than DCS buffer (%ld bytes)!",
			_sizeof_obj, _sizeof_obj_buffer );
#if 0
		// TODO: Following is being "optimized out" even when 
		// optimizations were turned off. Figure out why
		_obj_buffer = realloc( &_obj_buffer, _sizeof_obj );
#else
		if( _obj_buffer ) free( _obj_buffer );
		_obj_buffer = malloc( _sizeof_obj );
#endif
		_sizeof_obj_buffer = _sizeof_obj;
	}

	// Now rewind and the PNG file and load it into the _obj_buffer
	// from which it will be sent.
	rewind( fp );

	if( fread( _obj_buffer, sizeof(uint8_t), _sizeof_obj, fp ) != _sizeof_obj ) {
		object_interface = NULL;
		warn( "failed reading PNG from tmpfile" );
	}

	fclose( fp );

	return object_interface;
}


static int _control( void *buf, size_t len ) {
	return 0;
}


static struct data_source _P = {
	.get     = _get,
	.control = _control
};

/**
  * Default port number
  */
       unsigned int opt_max_silence_s = 10; // seconds
static unsigned short _port = 17071;
static const char *_addr = NULL;
       unsigned int _verbosity = 1;

static const char *USAGE
	= "%s -a <addr>[INET_ANY] -p <port>[%d] "
	"-w <width>[%d] -h <height>[%d] -f <format>[%s] "
	"-I <fixed RGB> -W <wait>[%d]\n";

static void _print_usage( const char *exename, FILE *fp ) {
	fprintf( fp, USAGE, 
		exename,
		_port,
		VIDEO_FORMATS[0].width,
		VIDEO_FORMATS[0].height,
		VIDEO_FORMATS[0].pixel_format, 
		opt_max_silence_s );
}


int main(int argc, char *argv[]) {

	int s = -1;
	int npx;

	srand( time(NULL) );

	do {
		static const char *OPTIONS
			= "a:p:w:h:f:W:v:?";
		const int c = getopt( argc, argv, OPTIONS );
		if( c < 0 ) break;

		switch(c) {

		case 'a':
			_addr = optarg;
			break;

		case 'p':
			_port = atoi( optarg );
			break;
		case 'w':
			VIDEO_FORMATS[0].width = atoi( optarg );
			break;

		case 'h':
			VIDEO_FORMATS[0].height = atoi( optarg );
			break;

		case 'f':
			strncpy( VIDEO_FORMATS[0].pixel_format, optarg, 5 );
			break;
		case 'v':
			_verbosity = atoi( optarg );
			break;

		case 'W':
			opt_max_silence_s = atoi( optarg );
			break;

		case '?':
			_print_usage( argv[0], stdout );
			exit(-1);
		default:
			printf ("error: unknown option: %c\n", c );
			exit(-1);
		}
	} while(1);

	/**
	  * Validate required arguments.
	  */


	/**
	  * Initialize the video subsystem (most likely to fail first).
	  */

	/**
	  * Select and open a device. Priority is given to device specified
	  * 1. on command line
	  * 2. in environment variable
	  * 3. found in the standard Linux /dev/video path
	  */
	static char device_path[ 256 ];

	if( optind < argc ) {
		strncpy( device_path, argv[ optind++ ], sizeof(device_path) );
	} else
	if( getenv("VIDEO_DEVICE") ) {
		strncpy( device_path, getenv("VIDEO_DEVICE"), sizeof(device_path) );
	} else
	if( first_video_dev( device_path, sizeof(device_path) ) ) {
		errx( -1, "no video device found" );
	}

	_vci = video_open( device_path );

	if( NULL == _vci ) {
		errx( -1, "failed opening %s", device_path );
	}

	/**
	  * Choose a format.
	  */
	{
		const int format_selector
			= _vci->config( _vci, VIDEO_FORMATS,
			sizeof(VIDEO_FORMATS)/sizeof(struct video_format) );
		if( format_selector != 0 /* ...because we only have one! */ ) {
			errx( -1, "failed configuring %s", device_path );
		}
	}
	_sizeof_vid_pixel = 2; // TODO: Assuming YUYV now...calculate properly!
	npx = VIDEO_FORMATS[0].width * VIDEO_FORMATS[0].height;

	_sizeof_vid_buffer
		= npx * _sizeof_vid_pixel;
	_sizeof_obj_buffer
		= npx * _samples_per_dcs_pixel;

	_vid_buffer
		= malloc( _sizeof_vid_buffer );
	if( _vid_buffer == NULL )
		err( -1, "allocating %ld-byte video buffer", _sizeof_vid_buffer );

	_obj_buffer
		= malloc( _sizeof_obj_buffer );
	if( _obj_buffer == NULL )
		err( -1, "allocating %ld-byte buffer for DCS conversion", _sizeof_obj_buffer );

	if( _obj_buffer )
		free( _obj_buffer );
	if( _vid_buffer )
		free( _vid_buffer );

	if( _vci )
		_vci->destroy( _vci );

	return 1;
}

