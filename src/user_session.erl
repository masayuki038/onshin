-module(user_session).
-export([loop/1]).

-include("onshin.hrl").

loop(Ets) ->
    receive
        {authenticate, {Pid, Input, Update}} ->
            #member{mail = Mail, password = Password, name = Name} = Input,
            lager:info("~p try authentication~n", [Name]),
            case check_parameters(Mail, Password, Name, Update) of
                ng -> Pid ! {unauthenticated, Input};
                ok ->
                    case storage:get_member(Mail) of
                        {ok, Member} ->
                            %% password check
                            #member{password = Password2} = Member,
                            case  Password =:= Password2 of
                                true -> 
                                    Session = #session{member = Member, md5 = erlang:md5(Mail), token = storage:generate_member_token(Mail)},
                                    ets:insert(Ets, {Session#session.token, Session}),
                                    lager:info("sessions: ~p~n", ets:tab2list(Ets)),
                                    Pid ! {authenticated, Session};
                                false ->
                                    Pid ! {unauthenticated, Input}
                            end;
                        {ng, _} ->
                            case Update of 
                                true -> 
                                    storage:update_member(Input),
                                    Session = #session{member = Input, md5 = erlang:md5(Mail), token = storage:generate_member_token(Mail)},
                                    ets:insert(Ets, {Session#session.token, Session}),
                                    Pid ! {authenticated, Session};
                                _ ->
                                    Pid ! {unauthenticated, Input}
                            end
                    end
            end,
            loop(Ets);
        {reconnect, {Pid, Mail, Token}} ->
            lager:info("~p(~p) reconnected~n", [Mail, Token]),
            case etc:lookup(Ets, Token) of
                [_, N] ->
                    lager:info("sessions: ~p~n", ets:tab2list(Ets)),
                    Pid ! {authenticated, N};
                [] ->
                    Pid ! {unauthenticated, #member{mail = Mail}}
            end,            
            loop(Ets);
        {quit, Token} -> 
            lager:info("~p left~n", [Token]),
            ets:delete(Ets, Token),
            lager:info("sessions: ~p~n", [ets:tab2list(Ets)]),
            loop(Ets)
    end.
         
check_parameters(Mail, Password, Name, Update) ->
    MailPasswordValid = {check_required(Mail), check_required(Password)},
    case MailPasswordValid of
        {ok, ok} -> 
            case Update of
                true -> check_required(Name);
                _ -> ok
            end;
        {_, _} -> ng
    end.

check_required(Param) ->
    case byte_size(Param) of
        0 -> ng;
        _ -> ok
    end.

