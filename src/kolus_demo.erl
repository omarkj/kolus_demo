-module(kolus_demo).

-export([start_backend/1]).

start_backend(Port) ->
    Dispatch = create_dispatch(kolus_demo_backend_handler, Port),
    {ok,P} = cowboy:start_http(erlang:make_ref(), 1024, [{port, Port}], [{dispatch, Dispatch}]),
    ets:insert(kolus_demo_backends, {backend, {{127,0,0,1},Port}}),
    {ok,P}.

% Internal
create_dispatch(Module, Port) ->
    [{'_', [{['...'], Module, [Port]}]}].
