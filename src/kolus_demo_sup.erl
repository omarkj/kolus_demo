
-module(kolus_demo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ets:new(kolus_demo_backends, [bag, named_table, public]),
    Dispatch = create_dispatch(kolus_demo_inbound_handler),
    {ok,_} = cowboy:start_http(inbound, 100, [{port, 10001}], [{dispatch, Dispatch}]),
    {ok, { {one_for_one, 5, 10}, []} }.

create_dispatch(Module) ->
    [{'_', [{['...'], Module, []}]}].
