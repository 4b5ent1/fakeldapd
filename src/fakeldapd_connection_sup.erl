-module(fakeldapd_connection_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    {ok,
     {
       {simple_one_for_one, 0, 1},
       [
        { fakeldapd_connection,
          {fakeldapd_connection, start_link, []},
          temporary, brutal_kill, worker,
          [fakeldapd_connection]
        }
       ]
     }
    }.
