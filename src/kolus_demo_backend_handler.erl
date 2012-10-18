-module(kolus_demo_backend_handler).

-export([init/3,
	 handle/2,
	 terminate/2]).

init(_Transport, Req, [Port]) ->
    {ok, Req, Port}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path_info(Req),
    handle(Path, Req1, State).

handle([<<"favicon.ico">>], Req, State) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<"404">>, Req),
    {ok, Req1, State};
handle(_Path, Req, State) ->
    {ok, Req1} = cowboy_req:reply(200, [], to_bin(State), Req),
    {ok, Req1, State}.

terminate(_,_) ->
    ok.

to_bin(I) when is_integer(I) ->
    to_bin(integer_to_list(I));
to_bin(L) when is_list(L) ->
    list_to_binary(L).
