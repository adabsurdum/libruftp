-module(snapshot).
-export([start/1, stop/0, init/1]).
-export([get_fragmented_image/1]).

-define(ASSERT,true).
-include_lib("stdlib/include/assert.hrl").

start(ExtPrg) ->
    Pid = spawn(?MODULE, init, [ExtPrg]),
	{ok,Pid,some_info}.

stop() ->
    snapproc ! stop.

get_fragmented_image( Fragsize ) ->
    call_port( Fragsize ).

call_port( Fragsize ) ->
    snapproc ! { call, self(), Fragsize },
    receive
	{ list_of_fragments, Fragments } ->
		?assert( is_list( Fragments ) ),
		?assert( lists:all( fun(El) -> is_binary(El) end, Fragments ) ),
		io:format( "~p fragments in list~n", [length(Fragments)]),
		?assert( lists:all( fun(El) -> (size(El) =:= Fragsize) end, lists:droplast(Fragments) ) ),
	    Fragments
    end.

init(ExtPrg) ->
    register( snapproc, self()),
    process_flag( trap_exit, true ),
    Port = open_port({spawn, ExtPrg}, [{packet,4}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Fragsize } ->
	    Port ! { self(), {command, term_to_binary(Fragsize)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! { list_of_fragments, binary_to_term(Data) }
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated,Reason)
    end.
