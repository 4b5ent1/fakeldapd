-module(fakeldapd_connection).

-behaviour(gen_server).

-include("LDAP.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([do_search/4, do_auth/5]).


-define(SERVER, ?MODULE).

-record(state,
        {socket,
         bind, %% {binding, Pid}, anonymous, {user, User}
         handlers,
         auth_timeout,
         search_timeout}).



start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).



init([Socket]) ->
    {ok, AuthTimeout} = application:get_env(fakeldapd, auth_timeout),
    {ok, SearchTimeout} = application:get_env(fakeldapd, search_timeout),

    process_flag(trap_exit, true),
    inet:setopts(Socket, [{active, once}]),
    { ok,
      #state{
         socket=Socket,
         bind=anonymous,
         handlers=dict:new(),
         auth_timeout=AuthTimeout,
         search_timeout=SearchTimeout}}.



handle_call({auth_result, ID, {ok, Name}}, {Pid, _Tag}, State = #state{socket=Socket, bind={binding, ID, Pid}}) ->
    send_response(
      Socket,
      ID,
      { bindResponse,
        #'BindResponse' {
           resultCode = success,
           matchedDN = "",
           diagnosticMessage = ""
          }
      }),

    {reply, ok, State#state{bind={user, Name}}};
handle_call({auth_result, ID, unavailable}, {Pid, _Tag}, State = #state{socket=Socket, bind={binding, ID, Pid}}) ->
    send_response(
      Socket,
      ID,
      { bindResponse,
        #'BindResponse' {
           resultCode = unavailable,
           matchedDN = "",
           diagnosticMessage = ""
        }
      }),

    {reply, ok, State#state{bind=anonymous}};
handle_call({auth_result, ID, _}, {Pid, _Tag}, State = #state{socket=Socket, bind={binding, ID, Pid}}) ->
    send_response(
      Socket,
      ID,
      { bindResponse,
        #'BindResponse' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
        }
      }),

    {reply, ok, State#state{bind=anonymous}};
handle_call({search_result_entry, ID, Entry}, {FromPid, _Tag}, State = #state{socket=Socket, handlers=Handlers}) ->
    case dict:find(ID, Handlers) of
        {ok, FromPid} ->
            send_response(Socket, ID, {searchResEntry, Entry});
        _ ->
            ok
    end,
    {reply, ok, State};
handle_call({search_result_done, ID, Result}, {FromPid, _Tag}, State = #state{socket=Socket, handlers=Handlers}) ->
    NewHandlers =
        case dict:find(ID, Handlers) of
            {ok, FromPid} ->
                send_response(Socket, ID, {searchResDone, Result}),
                dict:erase(ID, Handlers);
            _ ->
                Handlers
        end,
    {reply, ok, State#state{handlers=NewHandlers}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.



handle_cast({timeout, auth, ID}, State = #state{socket=Socket, bind={binding, ID, Pid}}) ->
    exit(Pid, timeout),
    send_response(
      Socket,
      ID,
      { bindResponse,
        #'BindResponse' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = "timeout"
        }
      }),

    {noreply, State#state{bind=anonymous}};
handle_cast({timeout, search, ID}, State = #state{socket=Socket, handlers=Handlers}) ->
    NewHandlers =
        case dict:find(ID, Handlers) of
            {ok, Pid} ->
                exit(Pid, timeout),
                send_response(Socket, ID, simple_response(searchResDone, timeLimitExceeded)),
                dict:erase(ID, Handlers);
            _ ->
                Handlers
        end,
    {noreply, State#state{handlers=NewHandlers}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info({tcp, Socket, Data}, State = #state{socket=Socket, bind=Bind}) ->
    case asn1rt:decode('LDAP', 'LDAPMessage', Data) of
        {ok, Message = #'LDAPMessage' {messageID=ID, controls=_}} ->
            case Bind of
                {binding, _, _} ->
                    gen_tcp:close(Socket),
                    {stop, request_while_binding, State};
                _ ->
                    Result = handle_msg(Message, State),
                    inet:setopts(Socket, [{active, once}]),
                    case Result of
                        {reply, Reply, NextState} ->
                            send_response(Socket, ID, Reply),
                            {noreply, NextState};
                        {stop, Reason, NextState} ->
                            gen_tcp:close(Socket),
                            {stop, Reason, NextState};
                        _ ->
                            Result
                    end
            end;
        {error, Reason} ->
            gen_tcp:close(Socket),
            {stop, Reason, State}
    end;
handle_info({tcp_closed, Socket}, State = #state{socket=Socket}) ->
    {stop, shutdown, State};
handle_info({tcp_error, Socket, Reason}, State = #state{socket=Socket}) ->
    {stop, {tcp_error, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




send_response(Socket, ID, Response) ->
    Message = #'LDAPMessage'{messageID=ID, protocolOp=Response},
    {ok, Data} = asn1rt:encode('LDAP', 'LDAPMessage', Message),
    gen_tcp:send(Socket, Data).


simple_response(Op, Code) ->
    { Op,
      #'LDAPResult' {
         resultCode = Code,
         matchedDN = "",
         diagnosticMessage = ""}
    }.



to_limit(0) ->
    infinity;
to_limit(X) ->
    X.


handle_msg(#'LDAPMessage' {protocolOp = {modifyRequest, _}}, State) ->
    {reply, simple_response(modifyResponse, invalidCredentials), State};
handle_msg(#'LDAPMessage' {protocolOp = {addRequest, _}}, State) ->
    {reply, simple_response(addResponse, invalidCredentials), State};
handle_msg(#'LDAPMessage' {protocolOp = {delRequest, _}}, State) ->
    {reply, simple_response(delResponse, invalidCredentials), State};
handle_msg(#'LDAPMessage' {protocolOp = {modDNRequest, _}}, State) ->
    {reply, simple_response(modDNResponse, invalidCredentials), State};
handle_msg(#'LDAPMessage' {protocolOp = {compareRequest, _}}, State) ->
    {reply, simple_response(compareDNResponse, invalidCredentials), State};
handle_msg(#'LDAPMessage' {protocolOp = {extendedReq, _}}, State) ->
    { reply,
      { extendedResp,
        #'ExtendedResponse' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};
handle_msg(#'LDAPMessage' {protocolOp={unbindRequest, 'NULL'}}, State) ->
    {stop, shutdown, State};



handle_msg(
  #'LDAPMessage' {
     protocolOp=
         { bindRequest,
           #'BindRequest' {
              version = 3,
              authentication = {sasl, _}
             }}},
  State = #state{socket=Socket, handlers=Handlers}) ->
    dict:map(
      fun (MessageID, HandlerPid) ->
              exit(HandlerPid, binding),
              send_response(Socket, MessageID, simple_response(searchResDone, operationsError))
      end,
      Handlers),

    { reply,
      { bindResponse,
        #'BindResponse' {
           resultCode = authMethodNotSupported,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State#state{bind=anonymous}
    };
handle_msg(
  #'LDAPMessage' {
     protocolOp=
         { bindRequest,
           #'BindRequest' {
              version = 3,
              name = [],
              authentication = {simple, []}
             }}},
  State = #state{socket=Socket, handlers=Handlers}) ->
    dict:map(
      fun (MessageID, HandlerPid) ->
              exit(HandlerPid, binding),
              send_response(Socket, MessageID, simple_response(searchResDone, operationsError))
      end,
      Handlers),

    { reply,
      { bindResponse,
        #'BindResponse' {
           resultCode = success,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State#state{bind=anonymous}
    };
handle_msg(
  #'LDAPMessage' {
     messageID = ID,
     protocolOp =
         { bindRequest,
           #'BindRequest' {
              version = 3,
              name = Name,
              authentication = {simple, Pass}
             }}},
  State = #state{
             socket=Socket,
             handlers=Handlers,
             auth_timeout=AuthTimeout}) ->
    dict:map(
      fun (MessageID, HandlerPid) ->
              exit(HandlerPid, binding),
              send_response(Socket, MessageID, simple_response(searchResDone, operationsError))
      end,
      Handlers),

    Timer =
        case AuthTimeout of
            0 ->
                none;
            infinity ->
                none;
            _ ->
                {ok, TRef} =
                    timer:apply_after(AuthTimeout, gen_server, cast, [self(), {timeout, auth, ID}]),
                TRef
        end,

    Pid = spawn_link(?MODULE, do_auth, [self(), Timer, ID, Name, Pass]),
    { noreply,
      State#state{
        bind={binding, ID, Pid},
        handlers=dict:new()}
    };
handle_msg(
  #'LDAPMessage' {
     messageID = ID,
     protocolOp =
         { searchRequest,
           #'SearchRequest' {timeLimit=TimeLimit} = Request
         }
    },
  State = #state{handlers=Handlers, search_timeout=SearchTimeout} ) ->
    case dict:is_key(ID, Handlers) of
        true ->
            { reply,
              { searchResDone,
                #'LDAPResult' {
                   resultCode = operationsError,
                   matchedDN = "",
                   diagnosticMessage = "Duplicate Message ID"
                  }
              },
              State};
        false ->
            Timeout = min(to_limit(TimeLimit), to_limit(SearchTimeout)),
            Timer =
                case Timeout of
                    infinity ->
                        none;
                    _ ->
                        {ok, TRef} =
                            timer:apply_after(Timeout, gen_server, cast, [self(), {timeout, search, ID}]),
                        TRef
                end,

            Pid = spawn_link(?MODULE, do_search, [self(), Timer, ID, Request]),
            { noreply,
              State#state{handlers=dict:store(ID, Pid, Handlers)}}
    end;
handle_msg(
  #'LDAPMessage' {
     protocolOp = {abandonRequest, ID}
    },
  State = #state{handlers=Handlers}) ->
    NewHandlers =
        case dict:find(ID, Handlers) of
            {ok, Pid} ->
                exit(Pid, abandon),
                dict:erase(ID, Handlers);
            error ->
                Handlers
        end,
    {noreply, State#state{handlers=NewHandlers}};
handle_msg(#'LDAPMessage' {messageID=ID, protocolOp=Op, controls=Ctls}, State) ->
    io:format("Msg: ~w ~w ~w~n", [ID, Op, Ctls]),
    {noreply, State}.



do_search(Conn, Timer, MessageID, Request) ->
    Table = gen_server:call(fakeldapd_table_manager, request_table),

    case Table of
        none ->
            Result =
                #'LDAPResult' {
                   resultCode = unavailable,
                   matchedDN = "",
                   diagnosticMessage = ""
                  },
            gen_server:call(Conn, {search_result_done, MessageID, Result});
        _ ->
            {ok, {M, F, A}} = application:get_env(fakeldapd, search_fun),
            Result = apply(M, F, [Conn, Table, MessageID, Request|A]),
            gen_server:call(Conn, {search_result_done, MessageID, Result}),
            case Timer of
                none ->
                    ok;
                _ ->
                    timer:cancel(Timer)
            end
    end.


do_auth(Conn, Timer, MessageID, Name, Pass) ->
    Table = gen_server:call(fakeldapd_table_manager, request_owner),

    case Table of
        none ->
            gen_server:call(Conn, {auth_result, MessageID, unavailable});
        _ ->

            {ok, {M, F, A}} = application:get_env(fakeldapd, auth_fun),
            Result = apply(M, F, [Table, Name, Pass|A]),
            gen_server:call(Conn, {auth_result, MessageID, Result}),
            case Timer of
                none ->
                    ok;
                _ ->
                    timer:cancel(Timer)
            end
    end.
