%%%-------------------------------------------------------------------
%% @doc dns top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dns_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{
        id => 0,
        start => {dns_server, start_link, [root]}
    },
    #{
        id => 1,
        start => {dns_server, start_link, [{name, "com"}]}
    },
    #{
        id => 2,
        start => {dns_server, start_link, [{name, "be"}]}
    },
    #{
        id => 3,
        start => {dns_server, start_link, [{name, "google.com"}]}
    },
    #{
        id => 4,
        start => {dns_server, start_link, [{name, "uclouvain.be"}]}
    },
    #{
        id => 10,
        start => {host, start_link, [{name, "www.google.com"}]}
    },
    #{
        id => 11,
        start => {host, start_link, [{name, "www.uclouvain.be"}]}
    },
    #{
        id => 20,
        start => {resolver, start_link, [resolver]}
    }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
