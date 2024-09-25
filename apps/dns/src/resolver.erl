-module(resolver).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1, resolve/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

resolve(Name) ->
    gen_server:call(resolver, {resolve, Name}).

resolve(Server, [H|T]) ->
    case dns_server:request(Server, H) of
        {ns, NS} -> resolve(NS, T);
        Pid -> Pid
    end.

start(Name) ->
    dns_sup:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({resolve, Name}, _From, State) ->
    {reply, resolve(root, Name), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
