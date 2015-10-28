
-module(webcam_server).

-export([run_server/1,server_loop/1]).

server_loop( WaiterArgs ) ->
	io:format("entered server_loop( ~p )~n", [WaiterArgs] ),
	receive
		_ -> io:format("server_loop got a message~n"),
		server_loop( WaiterArgs )
	end.


% Combining the boilerplate of both application and supervisor
% in this one module for now.
run_server(Args) -> 
	Pid = spawn_link( webcam_server, server_loop, [Args] ),
%	register('mushroom',Pid),
	{ok,Pid,some_info}.

