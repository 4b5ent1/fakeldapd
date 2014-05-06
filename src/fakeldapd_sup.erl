-module(fakeldapd_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).


init([Port]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs =
        [
         { fakeldapd_listener,
           {fakeldapd_listener, start_link, [Port]},
           permanent, brutal_kill, worker,
           [fakeldapd_listener]
         },

         { fakeldapd_connection_sup,
           {fakeldapd_connection_sup, start_link, []},
           permanent, brutal_kill, supervisor,
           [fakeldapd_connection_sup]
         },

         { fakeldapd_data_fetcher_sup,
           {fakeldapd_data_fetcher_sup, start_link, []},
           permanent, brutal_kill, supervisor,
           [fakeldapd_data_fetcher_sup]
         },

         { fakeldapd_table_owner_sup,
           {fakeldapd_table_owner_sup, start_link, []},
           permanent, brutal_kill, supervisor,
           [fakeldapd_table_owner_sup]
         },

         { fakeldapd_table_manager,
           {fakeldapd_table_manager, start_link, []},
           permanent, brutal_kill, worker,
           [fakeldapd_table_manager]
         }

        ],

    {ok, {SupFlags, ChildSpecs}}.
