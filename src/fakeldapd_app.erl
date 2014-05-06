-module(fakeldapd_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
    application:start(inets),
    application:start(asn1),
    application:start(parsetools),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(fakeldapd).


start(_StartType, _StartArgs) ->
    Port = application:get_env(fakeldapd, port, 389),
    fakeldapd_sup:start_link(Port).

stop(_State) ->
    ok.
