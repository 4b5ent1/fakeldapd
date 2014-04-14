-module(fakeldapd_listener).

-behaviour(supervisor_bridge).

-export([start_link/1]).

-export([init/1, terminate/2]).

-export([loop/1]).

-define(SERVER, ?MODULE).


start_link(Port) ->
    supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, [Port]).


init([Port]) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, asn1},
           {reuseaddr, true},
           {active, false}]),

    {ok, spawn_link(?MODULE, loop, [Socket]), Socket}.


loop(Socket) ->
    {ok, Conn} = gen_tcp:accept(Socket),

    case supervisor:start_child(fakeldapd_connection_sup, [Conn]) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Conn, Pid);
        {ok, Pid, _} ->
            gen_tcp:controlling_process(Conn, Pid);
        _ ->
            ok
    end,

    loop(Socket).


terminate(_Reason, _State) ->
    ok.
