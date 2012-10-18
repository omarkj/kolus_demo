-module(kolus_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    start(kolus_demo).

start(_StartType, _StartArgs) ->
    kolus_demo_sup:start_link().

stop(_State) ->
    ok.


start(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {not_started, Dep}} ->
	    start(Dep),
	    start(App)
    end.
	    
	    
