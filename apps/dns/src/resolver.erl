-module(resolver).
-behaviour(gen_server).

-define(INITIAL_TTL, 5).

%% API
-export([start/1, stop/1, start_link/1, resolve/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {cache}).

resolve(Name) ->
    NameList = utils:name_to_atoms(Name),
    gen_server:call(resolver, {resolve, NameList}).

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
    self() ! cache_clear,
    {ok, #state{cache=#{}}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({resolve, Name}, _From, #state{cache=Cache}) ->
    {Pid, TTL} = case maps:get(Name, Cache, errnotfound) of
        errnotfound -> {resolve(root, Name), ?INITIAL_TTL};
        P -> io:format("Serving from cache~n", []), P
    end,
    {reply, Pid, #state{cache=maps:put(Name, {Pid, TTL}, Cache)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cache_clear, #state{cache=Cache}) ->
    Self = self(),
    Cache2 = maps:map(fun (_, {Pid, TTL}) -> {Pid, TTL-1} end, Cache),
    Cache3 = maps:filter(fun (_, {_, TTL}) -> TTL =/= 0 end, Cache2),
    spawn(fun () -> timer:sleep(1000), Self ! cache_clear end),
    {noreply, #state{cache=Cache3}};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
