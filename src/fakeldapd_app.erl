-module(fakeldapd_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
    application:start(fakeldapd).


start(_StartType, _StartArgs) ->
    Port = application:get_env(fakeldapd, port, 389),
    fakeldapd_sup:start_link(Port).


stop(_State) ->
    ok.
