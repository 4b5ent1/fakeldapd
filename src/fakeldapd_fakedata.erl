-module(fakeldapd_fakedata).

-include("LDAP.hrl").

-export([fetch/0, search/4, auth/3]).




fetch() ->
    [
     {"admin", "Admin", "admin@example.com", "E10000", "10000"},
     {"joe",   "Joe",   "joe@example.com", "E10001", "10001"}
    ].



auth(Table, Name, Pass) ->
    {ok, OuDN} = application:get_env(fakeldapd, ouDN),
    Password =
        [
         {"admin@example.com", "secret"},
         {"joe@example.com", "password"}
        ],

    case parse_dn(Name) of
        {ok, [[{"uid", UID}]|OuDN]=DN} ->
            case ets:select(Table, [{{'$1', '_', '$3', '_', '_'}, [{'=:=', '$1', UID}], ['$3']}]) of
                [Mail] ->
                    case proplists:get_value(Mail, Password) of
                        Pass ->
                            {ok, DN};
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.



search(
  Conn, Table, MessageID,
  #'SearchRequest' {
     baseObject = BaseDN,
     scope = Scope,
     sizeLimit = SizeLimit,
     typesOnly = TypesOnly,
     filter = Filter,
     attributes = Attrs
    }) ->
    {ok, RootDN} = application:get_env(fakeldapd, rootDN),
    {ok, OuDN} = application:get_env(fakeldapd, ouDN),
    {ok, StaticEntries} = application:get_env(fakeldapd, static_entries),

    case parse_dn(BaseDN) of
        {ok, RootDN} ->
            case Scope of
                baseObject ->
                    {Code, Results} =
                        search_static_entries(
                          proplists:lookup_all(RootDN, StaticEntries),
                          Filter,
                          to_limit(SizeLimit)),

                    lists:foreach(
                      fun ({DN, Entry}) ->
                              ok = gen_server:call(
                                     Conn,
                                     { search_result_entry,
                                       MessageID,
                                       filter_entry(TypesOnly, Attrs, DN, Entry)})
                      end,
                      Results),

                    #'LDAPResult' {
                       resultCode = Code,
                       matchedDN = "",
                       diagnosticMessage = ""
                      };
                singleLevel ->
                    {Code, Results} =
                        search_static_entries(
                          StaticEntries,
                          Filter,
                          to_limit(SizeLimit)),

                    lists:foreach(
                      fun ({DN, Entry}) ->
                              ok = gen_server:call(
                                     Conn,
                                     { search_result_entry,
                                       MessageID,
                                       filter_entry(TypesOnly, Attrs, DN, Entry)})
                      end,
                      Results),

                    #'LDAPResult' {
                       resultCode = Code,
                       matchedDN = "",
                       diagnosticMessage = ""
                      };
                wholeSubtree ->
                    SizeLimit1 = to_limit(SizeLimit),
                    {Code1, Results1} = search_static_entries(StaticEntries, Filter, SizeLimit1),

                    lists:foreach(
                      fun ({DN, Entry}) ->
                              ok = gen_server:call(
                                     Conn,
                                     { search_result_entry,
                                       MessageID,
                                       filter_entry(TypesOnly, Attrs, DN, Entry)})
                      end,
                      Results1),

                    SizeLimit2 =
                        case SizeLimit1 of
                            infinity ->
                                infinity;
                            _ ->
                                SizeLimit1 - length(Results1)
                        end,

                    Code3 =
                        case Code1 of
                            sizeLimitExceeded ->
                                sizeLimitExceeded;
                            _ ->
                                {Code2, Results2} =
                                    search_account(Table, compile_account_filter(Filter), SizeLimit2),

                                lists:foreach(
                                  fun (Result) ->
                                          ok = gen_server:call(
                                                 Conn,
                                                 { search_result_entry,
                                                   MessageID,
                                                   format_account_result(TypesOnly, Attrs, Result, OuDN)})

                                  end,
                                  Results2),
                                Code2
                        end,

                    Code4 =
                        case {Code3, Code1} of
                            {sizeLimitExceeded, _} ->
                                sizeLimitExceeded;
                            {success, _} ->
                                success;
                            {_, success} ->
                                success;
                            {noSuchObject, noSuchObject} ->
                                noSuchObject
                    end,


                    #'LDAPResult' {
                       resultCode = Code4,
                       matchedDN = "",
                       diagnosticMessage = ""
                      }
                end;
        {ok, OuDN} ->
            case Scope of
                baseObject ->
                    {Code, Results} =
                        search_static_entries(
                          proplists:lookup_all(OuDN, StaticEntries),
                          Filter,
                          to_limit(SizeLimit)),

                    lists:foreach(
                      fun ({DN, Entry}) ->
                              ok = gen_server:call(
                                     Conn,
                                     { search_result_entry,
                                       MessageID,
                                       filter_entry(TypesOnly, Attrs, DN, Entry)})
                      end,
                      Results),

                    #'LDAPResult' {
                       resultCode = Code,
                       matchedDN = "",
                       diagnosticMessage = ""
                      };
                _ ->
                    SizeLimit1 = to_limit(SizeLimit),
                    {Code1, Results1} =
                        search_static_entries(
                          proplists:lookup_all(OuDN, StaticEntries),
                          Filter,
                          SizeLimit1),

                    lists:foreach(
                      fun ({DN, Entry}) ->
                              ok = gen_server:call(
                                     Conn,
                                     { search_result_entry,
                                       MessageID,
                                       filter_entry(TypesOnly, Attrs, DN, Entry)})
                      end,
                      Results1),

                    SizeLimit2 =
                        case SizeLimit1 of
                            infinity ->
                                infinity;
                            _ ->
                                SizeLimit1 - length(Results1)
                        end,

                    Code3 =
                        case Code1 of
                            sizeLimitExceeded ->
                                sizeLimitExceeded;
                            _ ->
                                {Code2, Results2} =
                                    search_account(Table, compile_account_filter(Filter), SizeLimit2),

                                lists:foreach(
                                  fun (Result) ->
                                          ok = gen_server:call(
                                                 Conn,
                                                 { search_result_entry,
                                                   MessageID,
                                                   format_account_result(TypesOnly, Attrs, Result, OuDN)})

                                  end,
                                  Results2),
                                Code2
                        end,

                    Code4 =
                        case {Code3, Code1} of
                            {sizeLimitExceeded, _} ->
                                sizeLimitExceeded;
                            {success, _} ->
                                success;
                            {_, success} ->
                                success;
                            {noSuchObject, noSuchObject} ->
                                noSuchObject
                    end,


                    #'LDAPResult' {
                       resultCode = Code4,
                       matchedDN = "",
                       diagnosticMessage = ""
                      }
            end;
        {ok, [[{"uid", UID}]|OuDN]} ->
            Guard = {'=:=', '$1', UID},
            Guard1 =
                case compile_account_filter(Filter) of
                    true ->
                        Guard;
                    false ->
                        false;
                    Filter1 ->
                        {'and', Guard, Filter1}
                end,

            {Code, Results} = search_account(Table, Guard1, infinity),
            lists:foreach(
              fun (Result) ->
                      ok = gen_server:call(
                             Conn,
                             { search_result_entry,
                               MessageID,
                               format_account_result(TypesOnly, Attrs, Result, OuDN)})
              end,
              Results),
            #'LDAPResult' {
               resultCode = Code,
               matchedDN = "",
               diagnosticMessage = ""
              };
        {ok, _} ->
            #'LDAPResult' {
               resultCode = noSuchObject,
               matchedDN = "",
               diagnosticMessage = ""
              };
        _ ->
            #'LDAPResult' {
               resultCode = invalidDNSyntax,
               matchedDN = "",
               diagnosticMessage = ""
              }
    end.



to_limit(0) ->
    infinity;
to_limit(X) ->
    X.



search_static_entries(Entries, Filter, SizeLimit) ->
    FilteredEntries =
        lists:filter(
          fun ({_, Entry}) ->
                  apply_static_filter(Filter, Entry)
          end,
          Entries),

    case FilteredEntries of
        [] ->
            {noSuchObject, []};
        _ ->
            case SizeLimit < length(FilteredEntries) of
                true ->
                    {sizeLimitExceeded, lists:sublist(FilteredEntries, SizeLimit)};
                false ->
                    {success, FilteredEntries}
            end
    end.



search_account(_Table, false, _SizeLimit) ->
    {noSuchObject, []};
search_account(Table, Guard, SizeLimit) ->
    Guard1 =
        case Guard of
            true ->
                [];
            _ ->
                [Guard]
        end,

    case SizeLimit of
        infinity ->
            case ets:select(Table, [{{'$1', '$2', '$3', '$4', '$5'}, Guard1, ['$_']}]) of
                '$end_of_table' ->
                    {noSuchObject, []};
                Results ->
                    {success, Results}
            end;
        0 ->
            case ets:select(Table, [{{'$1', '$2', '$3', '$4', '$5'}, Guard1, ['$_']}], 1) of
                '$end_of_table' ->
                    {noSuchObject, []};
                _ ->
                    {sizeLimitExceeded, []}
            end;
        _ ->
            case ets:select(Table, [{{'$1', '$2', '$3', '$4', '$5'}, Guard1, ['$_']}], SizeLimit) of
                '$end_of_table' ->
                    {noSuchObject, []};
                {Results, '$end_of_table'} ->
                    {success, Results};
                {Results, _} ->
                    {sizeLimitExceeded, Results}
            end
    end.



key_to_lower(Entry) ->
    [{string:to_lower(K), V} || {K,V} <- Entry].


apply_static_filter({'and', []}, _Entry) ->
    true;
apply_static_filter({'and', [Head|Tail]}, Entry) ->
    case apply_static_filter(Head, Entry) of
        false ->
            false;
        true ->
            apply_static_filter({'and', Tail}, Entry)
    end;
apply_static_filter({'or', []}, _Entry) ->
    false;
apply_static_filter({'or', [Head|Tail]}, Entry) ->
    case apply_static_filter(Head, Entry) of
        true ->
            true;
        false ->
            apply_static_filter({'or', Tail}, Entry)
    end;
apply_static_filter({equalityMatch, {'AttributeValueAssertion', Attr, Value}}, Entry) ->
    Attr1 = string:to_lower(Attr),
    Values = proplists:get_value(Attr1, key_to_lower(Entry)),

    case Values of
        undefined ->
            false;
        _ ->
            case Attr1 of
                "objectclass" ->
                    lists:member(string:to_lower(Value), lists:map(fun string:to_lower/1, Values));
                _ ->
                    lists:member(Value, Values)
            end
    end;
apply_static_filter({present, Attr}, Entry) ->
    proplists:is_defined(string:to_lower(Attr), key_to_lower(Entry));
apply_static_filter(_Filter, _Entry) ->
    false.



compile_account_filter({'and', []}) ->
    true;
compile_account_filter({'and', [Head|Tail]}) ->
    case compile_account_filter(Head) of
        false ->
            false;
        true ->
            compile_account_filter({'and', Tail});
        Head1 ->
            case compile_account_filter({'and', Tail}) of
                false ->
                    false;
                true ->
                    Head1;
                Tail1 ->
                    {'and', Head1, Tail1}
            end
    end;
compile_account_filter({'or', []}) ->
    false;
compile_account_filter({'or', [Head|Tail]}) ->
   case compile_account_filter(Head) of
       true ->
           true;
       false ->
           compile_account_filter({'or', Tail});
       Head1 ->
           case compile_account_filter({'or', Tail}) of
               true ->
                   true;
               false ->
                   Head1;
               Tail1 ->
                   {'or', Head1, Tail1}
           end
   end;
compile_account_filter({equalityMatch, {'AttributeValueAssertion', Attr, Value}}) ->
    case string:to_lower(Attr) of
        "objectclass" ->
            lists:member(string:to_lower(Value), ["posixaccount", "shadowaccount", "inetorgperson"]);
        "uid" ->
            {'=:=', '$1', Value};
        "cn" ->
            {'=:=', '$2', Value};
        "displayname" ->
            {'=:=', '$2', Value};
        "mail" ->
            {'=:=', '$3', Value};
        "employeenumber" ->
            {'=:=', '$4', Value};
        "uidnumber" ->
            {'=:=', '$5', Value};
        "gidnumber" ->
            {'=:=', '$5', Value};
        "loginshell" ->
            Value =:= "/bin/bash";
        "homedirectory" ->
            case Value of
                "/home/" ++ UID ->
                    {'=:=', '$1', UID};
                _ ->
                    false
            end
    end;
compile_account_filter({present, Attr}) ->
    lists:member(
      string:to_lower(Attr),
      ["objectclass", "uid", "cn", "displayname", "mail", "employeenumber", "uidnumber", "gidnumber", "loginshell", "homedirectory"]);
compile_account_filter(_Filter) ->
    false.




filter_entry_attrs(Attrs, Entry) ->
    case lists:map(fun string:to_lower/1, Attrs) of
        [] ->
            Entry;
        Attrs1 ->
            [ {K,V} || {K,V} <- Entry, lists:member(string:to_lower(K), Attrs1) ]
    end.


filter_entry_type(false, Entry) ->
    Entry;
filter_entry_type(true, Entry) ->
    [{K, []} || {K,_} <- Entry].


filter_entry(TypesOnly, Attrs, DN, Entry) ->
    Entry1 = filter_entry_type(TypesOnly, filter_entry_attrs(Attrs, Entry)),

    #'SearchResultEntry' {
       objectName=format_dn(DN),
       attributes=[#'PartialAttribute'{type=T, vals=V} || {T,V} <- Entry1]}.


format_account_result(TypesOnly, Attrs, Record, OuDN) ->
    {Uid, Name, Mail, EmployeeNumber, UidNumber} = Record,

    RawEntry =
        [
         {"objectClass",    ["inetOrgPerson", "posixAccount", "shadowAccount"]},
         {"uid",            [Uid]},
         {"cn",             [Name]},
         {"displayName",    [Name]},
         {"mail",           [Mail]},
         {"uidNumber",      [UidNumber]},
         {"gidNumber",      [UidNumber]},
         {"loginShell",     ["/bin/bash"]},
         {"homeDirectory",  ["/home/"++Uid]},
         {"employeeNumber", [EmployeeNumber]}
        ],

    filter_entry(
      TypesOnly,
      Attrs,
      [[{"uid", Uid}]|OuDN],
      RawEntry).


parse_dn(S) ->
    case dn_lexer:string(S) of
        {ok, Tokens, _} ->
            dn_parser:parse(Tokens);
        _ ->
            error
    end.



format_dn(DN) ->
    string:join(
      [
       string:join([ (T ++ "=" ++ V) || {T,V} <- RDN ], "+")
       || RDN <- DN
      ], ",").
