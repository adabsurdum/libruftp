
-module(ruftpsrv).

-export([run_server/1,waiter/1]).

waiter( WaiterArgs ) ->
	io:format("entered waiter( ~p )~n", [WaiterArgs] ),
	receive
		_ -> io:format("waiter got a message~n"),
		waiter( WaiterArgs )
	end.


% Combining the boilerplate of both application and supervisor
% in this one module for now.
run_server(Args) -> 
	Pid = spawn_link( ruftpsrv, waiter, [Args] ),
%	register('mushroom',Pid),
	{ok,Pid,some_info}.

