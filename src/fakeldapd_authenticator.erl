-module(fakeldapd_authenticator).

-behaviour(supervisor_bridge).

-export([start_link/4]).

-export([init/1, terminate/2]).

-export([auth/4, do_auth/4]).



start_link(Pid, MessageID, Name, Pass) ->
    supervisor_bridge:start_link(?MODULE, [Pid, MessageID, Name, Pass]).


init([Pid, MessageID, Name, Pass]) ->
    {ok, spawn_link(?MODULE, auth, [Pid, MessageID, Name, Pass]), {Pid, MessageID, Name}}.


auth(Pid, MessageID, Name, Pass) ->
    {ok, {M, F, A}} = application:get_env(fakeldapd, authFun),
    {ok, Timeout} = application:get_env(fakeldapd, authTimeout),
    spawn_link(?MODULE, do_auth, [self(), M, F, [Name, Pass|A]]),
    receive
        ok ->
            gen_server:cast(Pid, {auth_succeed, MessageID, Name});
        _ ->
            gen_server:cast(Pid, {auth_failure, MessageID})
    after
        Timeout ->
            ok
    end.


do_auth(Pid, M, F, A) ->
    link(Pid),
    Result = apply(M, F, A),
    Pid ! Result.


terminate(_Reason, _State) ->
    ok.
