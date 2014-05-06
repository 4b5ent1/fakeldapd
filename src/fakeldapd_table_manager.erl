-module(fakeldapd_table_manager).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        { owner }).



start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, Interval} = application:get_env(fakeldapd, refresh_interval),
    {ok, _} =
        timer:apply_interval(Interval, gen_server, cast, [self(), refresh]),
    gen_server:cast(self(), refresh),
    {ok, #state{owner=none}}.


handle_call(request_table, {FromPid, _Tag}, State=#state{owner=Owner}) ->
    Table =
        case Owner of
            none ->
                none;
            _ ->
                gen_server:call(Owner, {request_table, FromPid})
        end,
    {reply, Table, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(refresh, State) ->
    case supervisor:start_child(fakeldapd_data_fetcher_sup, [self()]) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok
    end,
    {noreply, State};
handle_cast({new_table_owner, NewOwner}, State = #state{owner=Owner}) ->
    case Owner of
        none ->
            ok;
        _ ->
            gen_server:call(Owner, release)
    end,
    {noreply, State#state{owner=NewOwner}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
