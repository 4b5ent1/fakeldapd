-module(fakeldapd_data_fetcher).

-behaviour(supervisor_bridge).

-export([start_link/1]).

-export([init/1, terminate/2]).

-export([fetch/1, do_fetch/4]).

-define(SERVER, ?MODULE).



start_link(ManagerPid) ->
    supervisor_bridge:start_link(?MODULE, [ManagerPid]).


init([ManagerPid]) ->
    {ok, spawn_link(?MODULE, fetch, [ManagerPid]), ManagerPid}.


fetch(ManagerPid) ->
    {ok, {M, F, A}} = application:get_env(fakeldapd, fetch_fun),
    {ok, Timeout} = application:get_env(fakeldapd, fetch_timeout),

    spawn_link(?MODULE, do_fetch, [self(), M, F, A]),

    receive
        OwnerPid ->
            gen_server:cast(ManagerPid, {new_table_owner, OwnerPid}),
            ok
    after
        Timeout ->
            exit(timeout)
    end.


do_fetch(ParentPid, M, F, A) ->
    Data = apply(M, F, A),

    Tab = ets:new(ldap_data, [set, {read_concurrency, true}]),
    ets:insert(Tab, Data),

    OwnerPid =
        case supervisor:start_child(fakeldapd_table_owner_sup, [ParentPid]) of
            {ok, Pid} ->
                Pid;
            {ok, Pid, _} ->
                Pid
        end,

    ets:give_away(Tab, OwnerPid, ok).


terminate(_Reason, _State) ->
    ok.
