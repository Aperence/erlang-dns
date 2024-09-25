%%% @author Author_Name <Author_Email>
%%% @copyright (C) 2024, Author_Name
%%% @doc 
%%%
%%% @end
%%% Created : 24 Sep 2024 by Author_Name <Author_Email>
-module(dns_server).

-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1, register/3, register_ns/3, request/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {names = #{}}).

request(Server, Name) ->
    gen_server:call(Server, {request, Name}).

register(Server, Name, Pid) ->
    gen_server:cast(Server, {register, Name, Pid}).

register_ns(Server, Name, Pid) ->
    gen_server:cast(Server, {register, Name, {ns, Pid}}).

start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [], []).

stop(Name) ->
    gen_server:call(Name, stop).

start_link({name, Name}) ->
    gen_server:start_link(?MODULE, [Name], []);

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

register_dns(Server, [T]) ->
    dns_server:register_ns(Server, T, self());

register_dns(Server, [H|T]) ->
    {ns, Pid} = dns_server:request(Server, H),
    register_dns(Pid, T).

init([]) ->
    {ok, #state{names=#{}}};

init([Name]) ->
    register_dns(root, Name),
    {ok, #state{names=#{}}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({request, Name}, _From, State=#state{names=Names}) ->
    {reply, maps:get(Name, Names, errnotfound), State}.

handle_cast({register, Name, Pid}, #state{names=Names}) ->
    {noreply, #state{names=maps:put(Name, Pid, Names)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

