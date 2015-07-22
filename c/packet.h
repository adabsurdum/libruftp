
/**
  * Constants defining the sizes and offsets of various fields in packets.
  */

#ifndef _packet_h_
#define _packet_h_

typedef uint32_t fragid_t;
typedef uint32_t reqid_t;
struct boolean_vector;

/**
  * Reliable UDP-based FTP is intended to share a port (and thus packet
  * namespace) with a higher level protocol (e.g. to control a webcam or
  * actually do FTP).
  */

#define RUFTP_BIT  (0x80)
#define RUFTP_MASK (0x07)

#define RUFTP_GET (RUFTP_BIT|0x01) // c  => s  triggers wave of PKT_DAT s => c

/**
  * Server sends one data packet.
  */
#define RUFTP_DAT (RUFTP_BIT|0x02) // c <=  s

/**
  * Client request resending of a specified set of packet indices.
  */
#define RUFTP_RES (RUFTP_BIT|0x03) // c  => s  server responds with series PKT_DAT s => c

/**
  * Client notifies server that client has received all content.
  * This is an optimization to allow the server to discard state associated
  * with the last request "early." Server will discard it anyway after a
  * timeout.
  */
#define RUFTP_ACK (RUFTP_BIT|0x04) // c  => s  client notifies server that PKT_DAT s => c

/**
  * Atomic protocol components, shared between multiple packets.
  */
#define SIZEOF_PKT_TYPE     (1)
#define SIZEOF_FLAGS        (1)
#define SIZEOF_FRAG_ID      (sizeof(fragid_t))
#define SIZEOF_MTU          (sizeof(uint16_t))
#define SIZEOF_REQ_ID       (sizeof(reqid_t))
#define SIZEOF_CRC16        (sizeof(uint16_t))
#define SIZEOF_PAYLOAD_SIZE (sizeof(uint16_t))

#define OFFSET_PKT_TYPE     (0)
#define OFFSET_PKT_FLAGS    (OFFSET_PKT_TYPE+SIZEOF_PKT_TYPE)
#define OFFSET_PAYLOAD_SIZE (OFFSET_PKT_FLAGS+SIZEOF_FLAGS)
#define OFFSET_RID          (OFFSET_PAYLOAD_SIZE+SIZEOF_PAYLOAD_SIZE)

/**
  * OFFSET_x constants below describe octet offsets from start of UDP packet
  * data to the specified item.
  * SIZEOF_x_FF is the number of octets of Fixed Fields in each packet type.
  *
  * ALL packets in this protocol include:
  * 1) a fixed header
  * 2) a 2-octet CRC tail.
  *
  * ALL packets EXCEPT ACK contain a variably-sized payload.
  */

// "GET"
#define OFFSET_GET_RID     (OFFSET_RID)
#define OFFSET_GET_MTU     (OFFSET_GET_RID+SIZEOF_REQ_ID)
#define OFFSET_GET_PAYLOAD (OFFSET_GET_MTU+SIZEOF_MTU)
#define SIZEOF_GET_FF      (OFFSET_GET_PAYLOAD+SIZEOF_CRC16)


// "DATa"
#define OFFSET_DAT_RID     (OFFSET_RID)
#define OFFSET_DAT_SOO     (OFFSET_DAT_RID+SIZEOF_REQ_ID)
#define OFFSET_DAT_FID     (OFFSET_DAT_SOO+sizeof(uint32_t))
#define OFFSET_DAT_PAYLOAD (OFFSET_DAT_FID+SIZEOF_FRAG_ID)
#define SIZEOF_DAT_FF      (OFFSET_DAT_PAYLOAD+SIZEOF_CRC16)

#define FLAG_TAIL_FRAG     (0x01)
#define MAX_SIZEOF_DAT_PAYLOAD(mtu) ((mtu)-SIZEOF_DAT_FF)

// "RESend"
#define OFFSET_RES_RID     (OFFSET_RID)
#define OFFSET_RES_PAYLOAD (OFFSET_RES_RID+SIZEOF_REQ_ID)
#define SIZEOF_RES_FF      (OFFSET_RES_PAYLOAD+SIZEOF_CRC16)


// "ACKnowledge"
#define OFFSET_ACK_RID     (OFFSET_RID)
#define OFFSET_ACK_CRC     (OFFSET_ACK_RID+SIZEOF_REQ_ID)
#define SIZEOF_ACK_FF      (OFFSET_ACK_CRC+SIZEOF_CRC16)

/**
  * Assuming DATa packets' sizes equal the MTU.
  */
static inline int min_packet_count( size_t sizeof_object, size_t mtu ) {
	const size_t SIZEOF_FRAGMENT
		= MAX_SIZEOF_DAT_PAYLOAD(mtu); // ...the packet payload capacity
	const size_t SIZEOF_REMAINDER
		= sizeof_object
		% SIZEOF_FRAGMENT;
	return (sizeof_object / SIZEOF_FRAGMENT)
		+ (SIZEOF_REMAINDER > 0 ? 1 : 0);
}

int build_packet_get( uint32_t rid, uint16_t mtu, uint8_t *buf, int buflen );
int build_packet_res( uint32_t rid, int max, struct boolean_vector *missing, uint8_t *buf, int buflen );
int build_packet_ack( uint32_t rid, uint8_t *buf, int buflen );

#endif

