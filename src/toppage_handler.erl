-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Html = get_html(),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Html, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

-spec get_html() -> any().
get_html() ->
    Host = econfig:get_value(onshin, "server", "host"),
    Port = econfig:get_value(onshin, "server", "port"),
    Prefix = helper:get_request_prefix(),
    PrivDir = code:priv_dir(onshin),
    Filename = filename:join([PrivDir, "index.html"]),
    {ok, Compiled} = sgte:compile_file(Filename),
    sgte:render(Compiled, [{host, Host}, {port, Port}, {prefix, Prefix}]).
