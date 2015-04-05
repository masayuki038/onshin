-module(websocket_handler).

-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-include("onshin.hrl").

init({tcp, http}, _Req, _Opts) ->
    lager:info("init/3"),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("websocket_init/3"),
    {ok, Req, undefined_state}.

websocket_handle({ping, Msg}, Req, State) ->
    lager:info("websocket_handle({ping, Msg})"),
    lager:info(Msg),
    {reply, {pong, Msg}, Req, State};

websocket_handle({text, Msg}, Req, State) ->
    lager:info("websocket_handle/3"),
    lager:info(Msg),
    Decoded = jsonx:decode(Msg),
    lager:info("Decoded: ~p~n", [Decoded]),
    case Decoded of
        {[{<<"event">>, <<"send_message">>}, {<<"data">>, {[{<<"message">>, Content}, {<<"user">>, User}]}}]} ->
            room ! {message, {Content, User}};
        {[{<<"event">>, <<"authenticate">>}, {<<"data">>, Data}]} ->
            {[{<<"mail">>, Mail}, {<<"password">>, Password}, {<<"name">>, Name}, {<<"update">>, Update}]} = Data,
            lager:info("user_session: ~p~n", [whereis(user_session)]),
            user_session ! {authenticate, {self(), #member{mail = Mail, password = Password, name = Name}, Update}};
        {[{<<"event">>, <<"reconnect">>}, {<<"data">>, {[{<<"mail">>, Mail}, {<<"token">>, Token}]}}]} ->
            user_session ! {reconnect, {self(), Mail, Token}}
    end,
    {ok, Req, State}.

websocket_info({publish, Messages}, Req, Session) ->
    lager:info("websocket_info({publish, Messages}, Req, State)"),
    lager:info("Messages: ~p", [Messages]),
    Encoded = jsonx:encode([{<<"event">>, <<"message">>}, {<<"data">>, to_binary_list(Messages)}]),
    lager:info("Encoded: ~p", [Encoded]),
    {reply, {text, Encoded}, Req, Session};
websocket_info({authenticated, Session}, Req, _State) ->
    lager:info("websocket_info({authenticated, Session}, Req, State)"),
    lager:info("Session: ~p~n", [Session]),
    Member = Session#session.member,
    lager:info("Member: ~p", [Member]),
    %% todo don't send mail address
    Encoded = jsonx:encode([{<<"event">>, <<"authenticated">>}, {<<"data">>, [{<<"token">>, Session#session.token}, {<<"mail">>, Member#member.mail}, {<<"name">>, Member#member.name}]}]),
    lager:info("Encoded: ~p", [Encoded]),
    room ! {join, {self(), Member#member.name}},
    {reply, {text, Encoded}, Req, Session};
websocket_info({unauthenticated, Member}, Req, State) ->
    lager:info("websocket_info({unauthenticated, Member}, Req, State)"),
    lager:info("Member: ~p", [Member]),
    Encoded = jsonx:encode([{<<"event">>, <<"unauthenticated">>}, {<<"data">>, [{<<"error">>, <<"Authentication Failed">>}]}]),
    {reply, {text, Encoded}, Req, State};
websocket_info({update_status, {EventType, Name}, States}, Req, Session) ->
    lager:info("websocket_info({update_status, Event, States}, Req, Session)"),
    Deployed = lists:map(fun({Member, Joined}) -> [{<<"member">>, Member}, {<<"joined">>, Joined}] end, States),
    Encoded = jsonx:encode([{<<"event">>, <<"update_status">>}, {<<"data">>, [{<<"event_type">>, EventType}, {<<"name">>, Name}, {<<"states">>, Deployed}]}]),
    lager:info("Encoded: ~p", [Encoded]),
    {reply, {text, Encoded}, Req, Session};    
websocket_info(_Info, Req, State) ->    
    lager:info("websocket_info/3"),
    {ok, Req, State}.
    
websocket_terminate(_Reason, _Req, _Session) -> 
    lager:info("websocket_terminate/3"),
    room ! {quit, self()},
    ok.

to_binary_list(Messages) ->
    to_binary_list(Messages, []).

to_binary_list([Message | Messages], Ret) ->
    {message, _, Content, User, At} = Message,
    to_binary_list(Messages, [{[{<<"message">>, Content}, {<<"user">>, User}, {<<"at">>, At}]} | Ret]);
to_binary_list([], Ret) ->
    Ret.
