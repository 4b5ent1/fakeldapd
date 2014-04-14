-module(fakeldapd_userdata).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state,
        { waiting,
          fetchTimeout,
          refreshInterval
        }).



start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, Timeout} = application:get_env(fakeldapd, fetchTimeout),
    {ok, RefreshInterval} = application:get_env(fakeldapd, refreshInterval),

    {ok, _} =
        timer:apply_interval(
          RefreshInterval,
          gen_server,
          cast,
          [self(), refresh]),

    ets:new(?MODULE, [bag, named_table]),
    {ok, #state{waiting=dict:new(), fetchTimeout=Timeout, refreshInterval=RefreshInterval}}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast({find_user, UID, From, MessageID}, State = #state{waiting=Waiting, fetchTimeout=Timeout}) ->
    case dict:is_key(UID, Waiting) of
        true ->
            Waiting1 = dict:append(UID, {From, MessageID}, Waiting),
            {noreply, State#state{waiting=Waiting1}};
        false ->
            case ets:lookup(?MODULE, UID) of
                [] ->
                    error_logger:info_msg("fakeldapd_userdata: start fetching data of user ~s~n", [UID]),

                    case supervisor:start_child(fakeldapd_userdata_fetcher_sup, [UID]) of
                        {ok, _} ->
                            ok;
                        {ok, _, _} ->
                            ok
                    end,

                    Waiting1 = dict:store(UID, [{From, MessageID}], Waiting),

                    timer:apply_after(
                      Timeout,
                      gen_server,
                      cast,
                      [self(), {timeout, UID}]),

                    {noreply, State#state{waiting=Waiting1}};
                [{_, _, LDIF}] ->
                    error_logger:info_msg("fakeldapd_userdata: cache hit ~s~n", [UID]),
                    gen_server:cast(From, {result, MessageID, UID, LDIF}),
                    {noreply, State};
                List ->
                    error_logger:info_msg("fakeldapd_userdata: multiple result ~s~n", [UID]),
                    Sorted = lists:keysort(2, List),
                    Latest = lists:last(Sorted),
                    ets:delete(?MODULE, UID),
                    ets:insert(?MODULE, Latest),
                    {_, _, LDIF} = Latest,
                    gen_server:cast(From, {result, MessageID, UID, LDIF}),
                    {noreply, State}
            end
    end;
handle_cast({found_user, UID, LDIF}, State = #state{waiting=Waiting}) ->
    Timestamp = ts2int(now()),
    ets:insert(?MODULE, {UID, Timestamp, LDIF}),
    error_logger:info_msg("fakeldapd_userdata: found user ~s~n", [UID]),
    Refs =
        case dict:find(UID, Waiting) of
            {ok, List} ->
                List;
            error ->
                []
        end,
    lists:map(
      fun({From, MessageID}) -> gen_server:cast(From, {result, MessageID, UID, LDIF}) end,
      Refs),
    Waiting1 = dict:erase(UID, Waiting),
    {noreply, State#state{waiting=Waiting1}};
handle_cast({timeout, UID}, State = #state{waiting=Waiting}) ->
    case dict:is_key(UID, Waiting) of
        true ->
            Waiting1= dict:erase(UID, Waiting),
            {noreply, State#state{waiting=Waiting1}};
        false ->
            {noreply, State}
    end;
handle_cast(refresh, State = #state{refreshInterval=RefreshInterval}) ->
    Timestamp = ts2int(now()),
    Number =
        ets:select_delete(
          ?MODULE,
          [{{'$1','$2', none},
            [{'>', {'-', Timestamp, '$2'}, RefreshInterval}],
            [true]}]),
    error_logger:info_msg("fakeldapd_userdata: deleted ~w not found user entries~n", [Number]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


ts2int({Meg, Sec, Micro}) ->
    (Meg*1000000+Sec)*1000+Micro/1000.
