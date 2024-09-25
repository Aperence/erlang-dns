-module(host).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

ping(Host) ->
    gen_server:call(Host, ping).

start(Name) ->
    dns_sup:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link({name, Name}) ->
    gen_server:start_link(?MODULE, [Name], []).

register_dns(Server, [T]) ->
    dns_server:register(Server, T, self());

register_dns(Server, [H|T]) ->
    {ns, Pid} = dns_server:request(Server, H),
    register_dns(Pid, T).

init([Name]) ->
    register_dns(root, Name),
    {ok, [#state{}]}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(ping, _From, State) ->
    {reply, pong, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
