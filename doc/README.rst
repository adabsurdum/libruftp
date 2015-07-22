
############################################################################
Message overview
############################################################################

1.	(G)et: initiates a request for a single *object* from the server.
	The format of the Get payload (request) is so far unspecified, but it
	is intended to be very generic.
2.	(D)ata: contains one *fragment* of data
3.	(R)esend: requests retransmission of a *set* of *fragments* identified by 
	their positions (0-based offset) in the object.
4.	(A)cknowledge: Tells the server it can discard state associated with REQUEST_ID.
	This is really just an optimization, since server will discard anyway after a
	timeout or as soon as a new request id is received. 

.. image:: ./messages.png
	:align: center

----

############################################################################
Message overview (cont)
############################################################################

1.	The overarching goal of the content of the (D)ata packet is to insure
	that the client can **determine** *from any packet* **the number of fragments
	to expect**, making this calculation independent of the order of packet
	arrival. Related to this...
2.	The (F) byte is flags, but only one flag is currently defined,
	**LAST_FRAGMENT**, and it is only applicable to (D)ata packets.
3.	The *only* packet type sent from server to client is (D)ata.
4.	PSIZE is *total* packet size in bytes.
5.	MTU is not intended to be the same as but should usually correspond to
	the TCP/IP MTU. Client might arbitrarily request extremely small
	packets be sent, well below the TCP/IP MTU for the route.

----

############################################################################
Client/Server roles
############################################################################

In the current implementation, only one request can be "in flight" at a
time. We can probably relax this, but it buys a lot of simplification that
shouldn't be given up without good reason. 

1.	Client should either ramdomly generate or increment REQUEST_ID.
	Since only one request is allowed (by the current implementation of
	my server) to be in progress at a time, it's also a session id.
2.	Server, on the initial GET, determines the size in bytes of requested
	object (e.g. image) and calculates a fragmentation that respects the
	client's (sent) MTU.
3.	The fragmentation will always consist of an ordered sequence of *fragments*
	all of which except the last *must* be the same FRAG_SIZE. 
4.	FRAG_SIZE == MTU-sizeof(fixed_data_packet_fields), so packet size is
	maximized subject to the given MTU.
5.	Unless sizeof(object) happens to be divisible by FRAG_SIZE,
	the last packet *and only the last packet* will be smaller.
6.	Client essentially maintains a bitvector indicating which fragments have been
	received and may send a (R)esend request after some timeout. Obviously, its
	handling of redundant fragments should be idempotent.


----

############################################################################
Fragmentation
############################################################################


``sizeof(object) = (N-1) * [MTU-sizeof(fixed_data_packet_fields)] + sizeof(last fragment)``

N is inferrable from every packet.

.. image:: ./fragmentation.png
	:align: center

----

############################################################################
State machines
############################################################################

.. image:: ./server_state.png
	:align: center
