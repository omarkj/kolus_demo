%%% -*- erlang -*-
-module(kolus_demo_tcp_transport).

-export([connect/3,
         recv/2, recv/3,
         send/2,
         setopts/2,
         controlling_process/2,
         peername/1,
         close/1,
         sockname/1]).

connect(Host, Port, Opts) when is_list(Host), is_integer(Port) ->
    case kolus:status(get_backends(ets:tab2list(kolus_demo_backends), [])) of
	[] ->
	    throw(no_backends);
	BackendStatus ->
	    case kolus:connect(<<"backend">>, hd(BackendStatus)) of
		{error, rejected} ->
		    throw(full);
		{socket, KSocket} ->
		    Socket = kolus:get_socket(KSocket),
		    inet:setopts(Socket, Opts ++ [binary, {packet, raw}]),
		    {ok, KSocket}
	    end
    end.

recv(KSocket, Length) ->
    recv(KSocket, Length, infinity).

recv(KSocket, Length, Timeout) ->
    Socket = kolus:get_socket(KSocket),
    gen_tcp:recv(Socket, Length, Timeout).

send(KSocket, Packet) ->
    Socket = kolus:get_socket(KSocket),
    gen_tcp:send(Socket, Packet).

setopts(KSocket, Opts) ->
    Socket = kolus:get_socket(KSocket),
    inet:setopts(Socket, Opts).

controlling_process(KSocket, Pid) ->
    Socket = kolus:get_socket(KSocket),
    gen_tcp:controlling_process(Socket, Pid).

peername(KSocket) ->
    Socket = kolus:get_socket(KSocket),
    inet:peername(Socket).

close(KSocket) ->
    Socket = kolus:get_socket(KSocket),
    case erlang:port_info(Socket) of
	undefined ->
	    kolus:finished(KSocket);
	_ ->
	    inet:setopts(Socket, [{active, false}]),
	    kolus:return(KSocket)
    end.

sockname(KSocket) ->
    Socket = kolus:get_socket(KSocket),
    inet:sockname(Socket).

% Internal
get_backends([], Res) ->
    Res;
get_backends([{backend,Backend}|Rest], Res) ->
    get_backends(Rest, Res ++ [Backend]).
