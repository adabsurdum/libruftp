-module(server).
-author('Roger Kramer <kramer@member.fsf.org>').

-export([listen/1,dispatch/0,dummy/1]).

% Packet type definitions.
-define(PKT_GET,16#46).
-define(PKT_RES,16#83).
-define(PKT_ACK,16#84).
-define(DAT_FIXED_FIELD_SIZE,18).

% Call echo:listen(Port) to start the service.
listen(Port) ->
	{ok, Socket} = gen_udp:open( Port, [binary,inet,{active,true}]),
	dispatch().

% Overall operation:
% When a GET is received 
% 1. an image is acquired and fragmented
% 2. all fragments are sent en masse
% 3. a child process is spawned to handle any RES/ACK follow-ons
% 4. If a RESend is received, all referenced packets are sent out
%    concurrently with the parsing of the fragment list.
% In other words, server keeps no state.

% 
fragment(Bytes,) ->
	{ok,0}.

% dispatch handles the initial GET request by spawning a process.
% ALL handling of the GET request occurs in the child process, except
% possibly some initial validation of the request (shouldn't spawn a child
% unless the request is valid).
dispatch() ->
	receive 
		{udp,Sock,Addr,Port,Data} ->
			%crc16(binary_part(Data,{0,size(Data)-2}),size(Data)-2),
			<<?PKT_GET,Flag,PSize:16,RID:32,MTU:16,Rem/binary>> = Data,
			%io:format("~p~n",[RID]),
			{ok,Image} = file:read_file('a.png'),
			{ok,Frags} = fragment( Image, MTU-?DAT_FIXED_FIELD_SIZE ),
			% TODO: validate the request!
			% Acquire an image, fragment it, and transmit the fragments on a
			% new socket, then spawn a child process to await RES/ACK
			% messages from the client.
			{ok, Reqsock} = gen_udp:open( 0, [binary,inet,{active,true}]),
			Reqproc = spawn_link(server,dummy,[ Data ]),
			gen_udp:controlling_process( Reqsock, Reqproc ),
			dispatch();
		_ ->
			io:format("Badly formed GET.~n"),
			dispatch()
	end.

% Just testing launch of RES/ACK handler.
dummy( Orig ) ->
	receive
		{udp,S,A,P,D} ->
			gen_udp:send( S, A, P, "From child:" ++ Orig  )
	end.

% This function waits for and handles RESend requests and ACKs, nothing
% else.
handler( Image ) ->
	receive
		{udp,Sock,Addr,Port,Data} ->
			ok;
		_ ->
			handler( Image )
	after 10000
		-> ok
	end.

