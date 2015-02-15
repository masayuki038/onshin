-module(room).
-export([loop/1]).
-export([update_status/1]).
-export([send_mail/3, convert_linefeed/1]).

-include("onshin.hrl").

loop(Clients) ->
    receive
        {join, {Pid, User}} ->
            lager:info("~p joined~n", [Pid]),
            NewClients = [{Pid, User} | Clients],
            publish([{Pid, User}], storage:get_messages()),
            ok = send_member_status({join, User}, NewClients),
            loop(NewClients);
        {message, {Content, User}} ->
            lager:info("~p(~p) received~n", [Content, User]),
            ok = send_message(Clients, Content, User),
            case re:run(Content, "^@([^\s]+).*$", [dotall, {capture, [1], list}]) of
                {match, Result} ->
                    [To | _] = Result,
                    lager:info("mention: ~p", [To]),
                    Members = storage:get_members(),
                    Filtered = lists:filter(fun(#member{name = Name}) -> To =:= binary_to_list(Name) end, Members),
                    lager:info("length(Filtered): ~p", [length(Filtered)]),
                    case length(Filtered) of
                        1 -> 
                            [SendFor | _] = Filtered,
                            #member{mail = Mail} = SendFor,  
                            Ret = send_mail(User, Mail, Content),
                            lager:info("send_mail: ~p", [Ret]);
                        _ -> 
                            lager:info("member not found: ~p", [To])
                    end;
                _ -> ok
            end, 
            loop(Clients);
        {reconnect, {Pid, Mail, Token}} ->
            lager:info("~p(~p) reconnected~n", [Mail, Token]),
            case storage:get_member(Mail) of
                {ok, Member} ->
                    case Token =:= Member#member.token of
                        true ->
                            Updated = Member#member{
                                token = storage:generate_member_token(Mail)
                            },
                            storage:update_member(Updated),
                            Pid ! {authenticated, Updated};
                        false ->
                            Pid ! {unauthenticated, #member{mail = Mail, token = Token}}
                    end;
                _ ->
                    Pid ! {unauthenticated, #member{mail = Mail, token = Token}}
            end,            
            loop(Clients);            
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
                                    Updated = Member#member{
                                        token = storage:generate_member_token(Mail)
                                    },
                                    storage:update_member(Updated),
                                    Pid ! {authenticated, Updated};
                                false ->
                                    Pid ! {unauthenticated, Input}
                            end;
                        {ng, _} ->
                            case Update of 
                                true -> 
                                    Updated = Input#member{
                                        token = storage:generate_member_token(Mail)
                                    },
                                    storage:update_member(Updated),
                                    Pid ! {authenticated, Updated};
                                _ ->
                                    Pid ! {unauthenticated, Input}
                            end
                    end
            end,
            loop(Clients);
        {quit, Pid} ->
            lager:info("~p left~n", [Pid]),
            {NewClients, Removed} = lists:partition(
                fun({E, _User}) -> not (Pid =:= E) end, Clients
            ),
            lists:foreach(fun({_R, User}) -> ok = send_member_status({quit, User}, NewClients) end, Removed),
            loop(NewClients)
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

send_message(Clients, Content, User) ->
    Message = storage:create_message(Content, User),
    storage:update_message(Message),
    publish(Clients, [Message]).

send_member_status(Event, Clients) ->
    States = update_status(Clients),
    lists:foreach(fun(Client) -> {Pid, _Member} = Client, Pid ! {update_status, Event, States} end, Clients),
    ok.

publish([Client | Clients], Messages) ->
    lager:info("publish([Pid | Pids], Message)"),
    lager:info("Client: ~p", [Client]),
    {Pid, _Receiver} = Client,
    Pid ! {publish, Messages},
    publish(Clients, Messages);
publish([], _Messages) ->
    ok.

update_status(Clients) ->
    Names = lists:map(fun(X) -> {_, User} = X, User end, Clients), 
    Members = lists:map(fun(X) -> #member{name = Name} = X, Name end, storage:get_members()),
    States = lists:map(fun(X) -> {X, lists:member(X, Names)} end, Members),
    lists:reverse(States).

send_mail(Sender, To, Message) ->
    lager:info("send_mail(Sender, To, Message)"),
    Host = econfig:get_value(onshin, "smtp", "host"),
    Port = econfig:get_value(onshin, "smtp", "port"),
 
    Subject = econfig:get_value(onshin, "smtp", "subject"),
    From = econfig:get_value(onshin, "smtp", "from"),

    SiteUrl = econfig:get_value(onshin, "smtp", "site_url"),
    Body = io_lib:format("Hi. You got a message from @~s.\r\n\r\n~s\r\n\r\nPlease check the site.\r\n~s", [Sender, convert_linefeed(Message), SiteUrl]),
    MailBody = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]),
    lager:info("MailBody: ~p", [MailBody]),
    gen_smtp_client:send_blocking({From, [To], MailBody}, [{relay, Host}, {port, Port}]).

convert_linefeed(Str) ->
     %%re:replace(Str, "(^|[^\r])\n", "\\1\r\n", [{return, list}, global]).
     re:replace(Str, "(^|[^\r])\n", "\\1\r\n", [{return, list}, multiline, global]).
