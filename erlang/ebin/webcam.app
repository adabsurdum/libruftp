
% A minimal .app file need only name the application and can have an empty
% options list.
{application, webcam, [
% An app involving a supervision tree-based app additionally needs a tuple
% with the 'mod' atom which names the module that exports the start/stop
% behaviour.
	{mod,          {webcam_app,[17071]}},
% An app packaged as a release by systools additionally requires the
% following 5:
	{description,  "Server of webcam imagery by RUFTP protocol"},
	{vsn,          "0.1.0"},
	{modules,      [webcam_app,webcam_server,snapshot]}, % ALL modules INTRODUCED by this app.
	{registered,   []}, % ALL names of processes registered by this app.
	{applications, [kernel,stdlib]}, % ...required BEFORE starting this app.
% Following are entirely (?) optional.
	{id,           "TODO:identifier"},
	{maxP,         infinity },
	{maxT,         infinity },
	{included_applications, []},
% Application configuration is {Par,Val} tuples in env retrievable at 
% runtime with application:get_env(App,Par)
	{env,          []},
%
	{runtime_dependencies, [] }]}.
% application name is needed by application:start,stop
% filename is used in application:load.
% iff they're the same then start can call load.
