-module(fakeldapd_table_owner).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {parent, table, clients, released}).



start_link(ParentPid) ->
    gen_server:start_link(?MODULE, [ParentPid], []).


init([ParentPid]) ->
    process_flag(trap_exit, true),
    {ok, #state{parent=ParentPid, table=none, clients=[], released=false}}.


handle_call(release, _From, State = #state{clients=[]}) ->
    {stop, shutdown, ok, State#state{released=true}};
handle_call(release, _From, State) ->
    {reply, ok, State#state{released=true}};
handle_call(
  {request_table, Client},
  _From,
  State = #state{table=Table, clients=Clients}) ->
    NewClients = link_client(Client, Clients),
    {reply, Table, State#state{clients=NewClients}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, _Reason}, State = #state{clients=Clients, released=false}) ->
    NewClients = lists:delete(Pid, Clients),
    {noreply, State#state{clients=NewClients}};
handle_info({'EXIT', Pid, _Reason}, State = #state{clients=Clients, released=true}) ->
    NewClients = lists:delete(Pid, Clients),
    case NewClients of
        [] ->
            {stop, shutdown, State#state{clients=NewClients}};
        _ ->
            {noreply, State#state{clients=NewClients}}
    end;
handle_info({'ETS-TRANSFER',Tab, _, _}, State = #state{parent=ParentPid, table=none}) ->
    ParentPid ! self(),
    {noreply, State#state{table=Tab}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



link_client(Client, Clients) ->
    case lists:member(Client, Clients) of
        true ->
            Clients;
        false ->
            link(Client),
            [Client|Clients]
    end.
