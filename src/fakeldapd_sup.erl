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

         { fakeldapd_userdata,
           {fakeldapd_userdata, start_link, []},
           permanent, brutal_kill, worker,
           [fakeldapd_userdata]
         },

         { fakeldapd_userdata_fetcher_sup,
           {fakeldapd_userdata_fetcher_sup, start_link, []},
           permanent, brutal_kill, supervisor,
           [fakeldapd_userdata_fetcher_sup]
         },

         { fakeldapd_authenticator_sup,
           {fakeldapd_authenticator_sup, start_link, []},
           permanent, brutal_kill, supervisor,
           [fakeldapd_authenticator_sup]
         },

         { fakeldapd_fakedata,
           {fakeldapd_fakedata, start_link, []},
           permanent, brutal_kill, worker,
           [fakeldapd_fakedata]
         }
        ],

    {ok, {SupFlags, ChildSpecs}}.
