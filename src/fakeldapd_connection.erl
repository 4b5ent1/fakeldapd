-module(fakeldapd_connection).

-behaviour(gen_server).

-include("ELDAPv3.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {socket,
         bind,
         requests,
         rootDN,
         rootLDIF,
         fetchTimeout,
         authTimeout}).



start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


init([Socket]) ->
    {ok, RootDN} = application:get_env(fakeldapd, rootDN),
    {ok, RootLDIF} = application:get_env(fakeldapd, rootLDIF),
    {ok, FetchTimeout} = application:get_env(fakeldapd, fetchTimeout),
    {ok, AuthTimeout} = application:get_env(fakeldapd, authTimeout),
    inet:setopts(Socket, [{active, once}]),
    { ok,
      #state{
         socket=Socket,
         bind=anonymous,
         requests=[],
         rootDN=RootDN,
         rootLDIF=RootLDIF,
         fetchTimeout=FetchTimeout,
         authTimeout=AuthTimeout
        }
    }.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({timeout, auth, ID}, State = #state{socket=Socket, requests={binding, ID}}) ->
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
    {noreply, State#state{bind=anonymous, requests=[]}};
handle_cast({timeout, fetch, _}, State = #state{requests={binding, _}}) ->
    {noreply, State};
handle_cast({timeout, fetch, ID}, State = #state{socket=Socket, requests=Requests}) ->
    case lists:member(ID, Requests) of
        false ->
            {noreply, State};
        true ->
            send_response(
              Socket,
              ID,
              { searchResDone,
                #'LDAPResult' {
                   resultCode = timeLimitExceeded,
                   matchedDN = "",
                   diagnosticMessage = ""
                  }
              }),
            {noreply, State#state{requests=lists:delete(ID, Requests)}}
    end;

handle_cast({auth_succeed, ID, Name}, State = #state{socket=Socket, requests={binding, ID}}) ->
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
    {noreply, State#state{bind={user, Name}, requests=[]}};
handle_cast({auth_failure, ID}, State = #state{socket=Socket, requests={binding, ID}}) ->
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
    {noreply, State#state{bind=anonymous, requests=[]}};
handle_cast({result, _, _, _}, State = #state{requests={binding, _}}) ->
    {noreply, State};
handle_cast({result, MessageID, UID, LDIF}, State = #state{socket=Socket, requests=Requests, rootDN=RootDN}) ->
    case lists:member(MessageID, Requests) of
        false ->
            {noreply, State};
        true ->
            case LDIF of
                none ->
                    send_response(
                      Socket,
                      MessageID,
                      { searchResDone,
                        #'LDAPResult' {
                           resultCode = noSuchObject,
                           matchedDN = "",
                           diagnosticMessage = ""
                          }
                      });
                _ ->
                    send_result_entry(Socket, MessageID, [[{"uid",UID}]|RootDN], LDIF),
                    send_response(
                      Socket,
                      MessageID,
                      { searchResDone,
                        #'LDAPResult' {
                           resultCode = success,
                           matchedDN = "",
                           diagnosticMessage = ""
                          }
                      })
            end,
            {noreply, State#state{requests=lists:delete(MessageID, Requests)}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({tcp, Socket, Data}, State = #state{socket=Socket, requests=Requests}) ->
    case asn1rt:decode('ELDAPv3', 'LDAPMessage', Data) of
        {ok, Message = #'LDAPMessage' {messageID=ID, controls=asn1_NOVALUE}} ->
            case Requests of
                {binding, _} ->
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
    {stop, normal, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




send_response(Socket, ID, Response) ->
    Message = #'LDAPMessage'{messageID=ID, protocolOp=Response},
    {ok, Data} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    gen_tcp:send(Socket, Data).


format_dn(DN) ->
    string:join(
      [
       string:join([ (T ++ "=" ++ V) || {T,V} <- RDN ], "+")
       || RDN <- DN
      ], ",").


send_result_entry(Socket, ID, DN, Attrs) ->
    send_response(
      Socket,
      ID,
      { searchResEntry,
        #'SearchResultEntry' {
           objectName=format_dn(DN),
           attributes=[#'PartialAttribute'{type=T, vals=V} || {T,V} <- Attrs]}
      }
     ).


parse_dn(S) ->
    case dn_lexer:string(S) of
        {ok, Tokens, _} ->
            dn_parser:parse(Tokens);
        _ ->
            error
    end.


handle_msg(
  #'LDAPMessage' {
     protocolOp=
         { bindRequest,
           #'BindRequest' {
              version = 3,
              name = [],
              authentication = {simple, []}
             }}},
  State) ->
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
  State = #state{socket=Socket, requests=Requests, rootDN=RootDN, fetchTimeout=Timeout}) ->
    lists:map(
      fun (MessageID) ->
              send_response(
                Socket,
                MessageID,
                { searchResDone,
                  #'LDAPResult' {
                     resultCode = operationsError,
                     matchedDN = "",
                     diagnosticMessage = "Duplicate Message ID"
                    }
                })
      end,
      Requests),

    case parse_dn(Name) of
        {ok, [[{"uid", UID}]|RootDN]} ->
            case supervisor:start_child(fakeldapd_authenticator_sup, [self(), ID, UID, Pass]) of
                {ok, _} ->
                    ok;
                {ok, _, _} ->
                    ok
            end,

            timer:apply_after(
              Timeout,
              gen_server,
              cast,
              [self(), {timeout, auth, ID}]),

            {noreply, State#state{requests={binding, ID}}};
        _ ->
            { reply,
              { bindResponse,
                #'BindResponse' {
                   resultCode = invalidCredentials,
                   matchedDN = "",
                   diagnosticMessage = ""
                  }
              },
              State#state{requests=[]}}
    end;
handle_msg(#'LDAPMessage' {protocolOp={unbindRequest, 'NULL'}}, State) ->
    {stop, normal, State};
handle_msg(
  #'LDAPMessage' {
     messageID = ID,
     protocolOp = { searchRequest, #'SearchRequest' {} = Request }
    },
  State = #state{requests=Requests} ) ->
    case lists:member(ID, Requests) of
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
            handle_search(ID, Request, State)
    end;
handle_msg(
  #'LDAPMessage' {
     protocolOp = {abandonRequest, ID}
    },
  State = #state{requests=Requests}) ->
    {noreply, State#state{requests=lists:delete(ID, Requests)}};

handle_msg(
  #'LDAPMessage' {
     protocolOp = { modifyRequest, _}
    },
  State) ->
    { reply,
      { modifyResponse,
        #'LDAPResult' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};
handle_msg(
  #'LDAPMessage' {
     protocolOp = { addRequest, _}
    },
  State) ->
    { reply,
      { addResponse,
        #'LDAPResult' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};
handle_msg(
  #'LDAPMessage' {
     protocolOp = { delRequest, _}
    },
  State) ->
    { reply,
      { delResponse,
        #'LDAPResult' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};
handle_msg(
  #'LDAPMessage' {
     protocolOp = { modDNRequest, _}
    },
  State) ->
    { reply,
      { modDNResponse,
        #'LDAPResult' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};
handle_msg(
  #'LDAPMessage' {
     protocolOp = { compareRequest, _}
    },
  State) ->
    { reply,
      { compareResponse,
        #'LDAPResult' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};

handle_msg(
  #'LDAPMessage' {
     protocolOp = { extendedReq, _}
    },
  State) ->
    { reply,
      { extendedResp,
        #'ExtendedResponse' {
           resultCode = invalidCredentials,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State};
handle_msg(#'LDAPMessage' {messageID=ID, protocolOp=Op, controls=Ctls}, State) ->
    io:format("Msg: ~w ~w ~w~n", [ID, Op, Ctls]),
    {noreply, State}.



handle_search(
  ID,
  #'SearchRequest' {
     baseObject = BaseDN,
     scope = baseObject
    },
  State = #state{socket=Socket, requests=Requests, rootDN=RootDN, rootLDIF=RootLDIF, fetchTimeout=Timeout}) ->
    case parse_dn(BaseDN) of
        {ok, RootDN} ->
            send_result_entry(Socket, ID, RootDN, RootLDIF),
            { reply,
              { searchResDone,
                #'LDAPResult' {
                   resultCode = success,
                   matchedDN = "",
                   diagnosticMessage = ""
                  }
              },
              State};
        {ok, [[{"uid", UID}]|RootDN]} ->
            gen_server:cast(fakeldapd_userdata, {find_user, UID, self(), ID}),

            timer:apply_after(
              Timeout,
              gen_server,
              cast,
              [self(), {timeout, fetch, ID}]),

            {noreply, State#state{requests=Requests++[ID]}};
        {ok, _} ->
            { reply,
              { searchResDone,
                #'LDAPResult' {
                   resultCode = noSuchObject,
                   matchedDN = "",
                   diagnosticMessage = ""
                  }
              },
              State};
        _ ->
            { reply,
              { searchResDone,
                #'LDAPResult' {
                   resultCode = invalidDNSyntax,
                   matchedDN = "",
                   diagnosticMessage = ""
                  }
              },
              State}
    end;


handle_search(
  _ID,
  _Req,
  State) ->
    { reply,
      { searchResDone,
        #'LDAPResult' {
           resultCode = noSuchObject,
           matchedDN = "",
           diagnosticMessage = ""
          }
      },
      State}.
