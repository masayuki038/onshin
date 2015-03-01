-module(onshin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    register(room, spawn(fun() -> room:loop([]) end)),
    Ets = ets:new(sessions, [public]),
    register(user_session, spawn(fun() -> user_session:loop(Ets) end)),
    storage:start(disc_copies),
    ok = bootstrap_cowboy(),
    onshin_sup:start_link().

stop(_State) ->
    ok.

bootstrap_cowboy() ->
    application:set_env(lager, handlers, [
        {lager_console_backend, info},
        {lager_file_backend, [
            {"error.log", error, 10485760, "$D0", 5},
            {"console.log", info, 10485760, "$D0", 5}
        ]}
    ]),
    ok = econfig:register_config(onshin, ["./onshin.ini"], [autoreload]),
    true = econfig:subscribe(onshin),
    Prefix = helper:get_request_prefix(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {Prefix ++ "/", toppage_handler, []},
            {Prefix ++ "/websocket", websocket_handler, []},
            {Prefix ++ "/static/[...]", cowboy_static, {priv_dir, onshin, <<"static">>}}
        ]}
    ]),
    Port = econfig:get_value(onshin, "server", "port"),
    {ok, _} = cowboy:start_http(sample_http_handler, 100, [{port, list_to_integer(Port)}], [{env, [{dispatch, Dispatch}]}]),
    ok.
