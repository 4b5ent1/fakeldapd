-module(fakeldapd_userdata_fetcher).

-behaviour(supervisor_bridge).

-export([start_link/1]).

-export([init/1, terminate/2]).

-export([fetch/1, do_fetch/4]).



start_link(UID) ->
    supervisor_bridge:start_link(?MODULE, [UID]).


init([UID]) ->
    {ok, spawn_link(?MODULE, fetch, [UID]), UID}.


fetch(UID) ->
    {ok, {M, F, A}} = application:get_env(fakeldapd, fetchFun),
    {ok, Timeout} = application:get_env(fakeldapd, fetchTimeout),
    spawn_link(?MODULE, do_fetch, [self(), M, F, [UID|A]]),
    receive
        LDIF ->
            gen_server:cast(fakeldapd_userdata, {found_user, UID, LDIF})
    after
        Timeout ->
            ok
    end.


do_fetch(Pid, M, F, A) ->
    link(Pid),
    LDIF = apply(M, F, A),
    Pid ! LDIF.


terminate(_Reason, _State) ->
    ok.
