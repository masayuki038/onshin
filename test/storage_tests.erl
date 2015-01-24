-module(storage_tests).
-include_lib("eunit/include/eunit.hrl").
-include("message.hrl").

get_members_test_() ->
    {setup, fun start/0, fun stop/1,
        fun(_) ->
            Member1 = create_member1(),
            Member2 = create_member2(),
            storage:update_member(Member1),
            storage:update_member(Member2),
            Members = storage:get_members(),
            ?_assertEqual(2, length(Members)),
            [H | _] = Members,
            L = lists:last(Members),
            ?_assertEqual(Member2, H),
            ?_assertEqual(Member1, L)
        end
    }.

create_member1() ->
    #member{mail = "foo@bar.com", password = "foo", name = "Foo"}.

create_member2() ->
    #member{mail = "hoge@hoge.co.jp", password = "hoge", name = "Egoh"}.

start() ->
    ?debugMsg("storage_test:start called"),
    storage:clean_start(ram_copies).

stop(_) ->
    ?debugMsg("storage_test:stop called"),
    storage:stop().


