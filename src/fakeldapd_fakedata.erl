-module(fakeldapd_fakedata).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([fetch/1, auth/2]).

-define(SERVER, ?MODULE).

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, #state{}}.


handle_call({fetch, UID}, _From, State) ->
    error_logger:info_msg("fakedata: fetching ~s~n", [UID]),
    {ok, LDIFs} = application:get_env(fakeldapd, fakeUserData),
    case proplists:lookup(UID, LDIFs) of
        none ->
            {reply, none, State};
        {UID, LDIF} ->
            {reply, LDIF, State}
    end;
handle_call({auth, Name, Pass}, _From, State) ->
    error_logger:info_msg("fakedata: authenticating ~s~n", [Name]),
    {ok, Users} = application:get_env(fakeldapd, fakeAuthData),
    case proplists:lookup(Name, Users) of
        {Name, Pass} ->
            {reply, ok, State};
        _ ->
            {reply, fail, State}
    end.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


fetch(UID) ->
    gen_server:call(?SERVER, {fetch, UID}).


auth(Name, Pass) ->
    gen_server:call(?SERVER, {auth, Name, Pass}).
