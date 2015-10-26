-module(ruftpcam).

% Combining the boilerplate of both application and supervisor
% in this one module for now.
-behavior(application).
-behavior(supervisor).

% Application behavior exports.
-export([start/2,stop/1]).

% Supervisor behavior exports.
-export([init/1]).

%-compile([nowarn_unused_function,{spinner,0}]).
% May be preferrable to just export it as compiler may drop it(?)
%-compile(export_all).

%-record(aggregation,{ready=false,pending=0,delinquent=0}).

%loop(pending,avail,State) ->
%loop(pending,unavail) ->
%	ok.

% application callback
start(normal,StartArgs) ->
	io:format("ruftpcam:start(~p)~n",[StartArgs]),
	supervisor:start_link({local,server_sup},ruftpcam, StartArgs).
% ...supervisor process will look for init in ruftpcam.


% If I understand things correctly this is called *after* Erlang has
% already stopped descendents. In other words, this is a notification, and
% it is not obligated itself to stop descendents(?)
% application callback
stop(State) ->
	io:format("ruftpcam:stop(~p)~n",[State]),
	ok.
%	supervisor:terminate_child(server_sup,child_name).
% ...the child being terminated is actually the app's supervisor which
% itself will terminate the descendent workers.

%	unregister('mushroom').


% This returns a set of "childspecs" describing the processors that
% the supervisor will supervise.
% supervisor callback
% Called by the process started above by supervisor:start_link to find out
% what children (workers) need to be spun up and where they reside...
init(StartArgs) ->
	SupervisorFlags = #{strategy => one_for_one,
		intensity => 1,
		period => 5 },
	ChildSpecs = [#{id => ruftpsrv_id,
		start => { ruftpsrv, run_server, StartArgs},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ruftpsrv]}],
	{ ok, { SupervisorFlags, ChildSpecs }}.

