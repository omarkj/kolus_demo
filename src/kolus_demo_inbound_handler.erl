-module(kolus_demo_inbound_handler).

-include_lib("kolus/include/kolus.hrl").

-export([init/3,
	 handle/2,
	 terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path_info(Req),
    handle(Path, Req1, State).

handle([<<"favicon.ico">>], Req, State) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<"404">>, Req),
    {ok, Req1, State};
handle([<<"backends">>], Req, State) ->
    BackendStatus = kolus:status(get_backends(ets:tab2list(kolus_demo_backends), [])),
    {ok, Req1} = cowboy_req:reply(200, [], format_reply(BackendStatus, <<>>), Req),
    {ok, Req1, State};
handle([], Req, State) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<"404">>, Req),
    {ok, Req1, State};
handle([<<"one">>], Req, State) ->
    Time = case cowboy_req:qs_val(<<"time">>, Req) of
	       {undefined,_} ->
		   0;
	       {Time0, _} ->
		   list_to_integer(binary_to_list(Time0))
	   end,
    TimeBin = list_to_binary(integer_to_list(Time)),
    case kolus:status(get_backends(ets:tab2list(kolus_demo_backends), [])) of
	[] ->
	    {ok, Req0} = cowboy_req:reply(503, [], <<>>, Req),
	    {ok, Req0, State};
	BackendStatus ->
	    {ok, Req0} = cowboy_req:chunked_reply(200, Req),
	    {socket, KSocket} = kolus:connect(<<"backend">>, hd(BackendStatus)),
	    Socket = kolus:get_socket(KSocket),
	    SocketBin = list_to_binary(erlang:port_to_list(Socket)),
	    ok = cowboy_req:chunk(<<"Going to hold on to socket ",
				    SocketBin/binary, " for ",
				    TimeBin/binary, " ms\r\n">>, Req0),
	    timer:sleep(Time),
	    ok = kolus:return(KSocket),
	    ok = cowboy_req:chunk(<<"Returned socket ", SocketBin/binary, "\r\n">>, Req0),
	    {ok, Req0, State}
    end;
handle([<<"two">>], Req, State) ->
    Time = case cowboy_req:qs_val(<<"time">>, Req) of
	       {undefined,_} ->
		   0;
	       {Time0, _} ->
		   list_to_integer(binary_to_list(Time0))
	   end,
    try
	{ok, Client0} = connect_backend(kolus_demo_tcp_transport),
	{ok, Body, Client1} = hackney:body(Client0),
	{ok, Req1} = cowboy_req:chunked_reply(200, Req),
	ok = cowboy_req:chunk(<<"Got connected to backend ", Body/binary, "\r\n">>, Req1),
	timer:sleep(Time),
	hackney:close(Client1),
	{ok, Req1, State}
    catch
	no_backends ->
	    {ok, Req0} = cowboy_req:reply(503, [], <<>>, Req),
	    {ok, Req0, State};
	full ->
	    {ok, Req0} = cowboy_req:reply(502, [], <<>>, Req),
	    {ok, Req0, State}
    end;
handle([<<"three">>], Req, State) ->
    Time = case cowboy_req:qs_val(<<"time">>, Req) of
	       {undefined,_} ->
		   0;
	       {Time0, _} ->
		   list_to_integer(binary_to_list(Time0))
	   end,
    try
	{ok, Client0} = connect_backend(kolus_demo_idle_first_tcp_transport),
	{ok, Body, Client1} = hackney:body(Client0),
	{ok, Req1} = cowboy_req:chunked_reply(200, Req),
	ok = cowboy_req:chunk(<<"Got connected to backend ", Body/binary, "\r\n">>, Req1),
	timer:sleep(Time),
	hackney:close(Client1),
	{ok, Req1, State}
    catch
	no_backends ->
	    {ok, Req0} = cowboy_req:reply(503, [], <<>>, Req),
	    {ok, Req0, State};
	full ->
	    {ok, Req0} = cowboy_req:reply(502, [], <<>>, Req),
	    {ok, Req0, State}
    end.

terminate(_,_) ->
    ok.

% Internal
connect_backend(Module) ->
    Client = get_client(Module),
    case hackney:send_request(Client, {get, <<"/">>, [], <<>>}) of
	{error, closed} ->
	    hackney:close(Client),
	    connect_backend(Module);
	{ok, _, _, Client0} ->
	    {ok, Client0}
    end.

get_client(Module) ->
    {ok, Client} = hackney:connect(Module,
				   "localhost", % Not important for this test.
				   80), % Not important for this test
    Client.

get_backends([], Res) ->
    Res;
get_backends([{backend,Backend}|Rest], Res) ->
    get_backends(Rest, Res ++ [Backend]).

format_reply([], Res) ->
    Res;
format_reply([#kolus_backend{ip=Ip,port=Port,
			     idle=Idle,unused=Unused}|Rest], Res) ->
    Res0 = <<"Remote: ", (to_bin(Ip))/binary, ":", (to_bin(Port))/binary, "\n",
	     "Idle:   ", (to_bin(Idle))/binary, "\n",
	     "Unused: ", (to_bin(Unused))/binary, "\n\n">>,
    format_reply(Rest, <<Res/binary, Res0/binary>>).

to_bin(List) when is_list(List) ->
    list_to_binary(List);
to_bin(Atom) when is_atom(Atom) ->
    to_bin(atom_to_list(Atom));
to_bin(Integer) when is_integer(Integer) ->
    to_bin(integer_to_list(Integer));
to_bin({A,B,C,D}) ->
    binary:list_to_bin([to_bin(A),<<".">>,
			to_bin(B),<<".">>,
			to_bin(C),<<".">>,
			to_bin(D)]).




    
