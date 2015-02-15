-module(storage).
-compile(export_all).

-include("onshin.hrl").
-include_lib("stdlib/include/qlc.hrl").

clean_start(DbType) ->
    Node = node(),
    mnesia:delete_schema([Node]),
    start(DbType).

start(DbType) ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok ->
            prepare_db(true, Node, DbType);
        {error, {Node, {already_exists, Node}}} ->
            prepare_db(false, Node, DbType); 
        _ -> error
    end.

stop() ->
    mnesia:stop().

-spec prepare_db(_, _, _) -> any().
prepare_db(CreateTables, Node, DbType) ->
    ok = mnesia:start(),
    case CreateTables of
        true -> 
            ok = create_tables(Node, DbType);
        false -> ok
     end,
    mnesia:wait_for_tables([message], 20000).

-spec create_tables(_, _) -> ok.
create_tables(Node, DbType) ->
    mnesia:create_table(message, [{attributes, record_info(fields, message)}, {DbType, [Node]}]),
    mnesia:create_table(member, [{attributes, record_info(fields, member)}, {DbType, [Node]}]),
    ok.

-spec create_message(undefined | binary(), _) -> message().
create_message(Content, User) ->
    Now = erlang:localtime(),
    #message{message_id = generate_message_id(Content, Now), content = Content, user = User, at = iso8601:format(Now)}.

-spec generate_message_id(binary(), _) -> [integer()]. 
generate_message_id(Content, Seed) ->
    Md5_bin = erlang:md5(lists:concat([binary_to_list(Content), ";", tuple_to_list_deeply(Seed)])),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

-spec get_messages() -> list().
get_messages() ->
    take(10, do(qlc:sort(qlc:q([X || X <- mnesia:table(message)]),[{order,  
        fun(M1, M2) ->
            #message{at = At1} = M1,
            #message{at = At2} = M2,
            At1 > At2
        end}])
    )).

take(0, _) -> [];
take(_, []) -> [];
take(N, [X | Xs]) when N > 0 -> [X | take(N-1, Xs)].

-spec do(_) -> any().
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
               
-spec get_members() -> list().
get_members() ->
    take(5, do(qlc:sort(qlc:q([X || X <- mnesia:table(member)]),[{order,  
        fun(M1, M2) ->
            #member{name = Name1} = M1,
            #member{name = Name2} = M2,
            Name1 < Name2
        end}])
    )).

-spec update_message(_) -> {aborted, _} | {atomic, _}.
update_message(M) ->
    mnesia:transaction(fun() ->mnesia:write(M) end).

-spec get_member(_) -> any().
get_member(Mail) ->
    F = fun() -> mnesia:read({member, Mail}) end,
    {atomic, Val} = mnesia:transaction(F),
    case length(Val) of
        1 ->
            [H | _] = Val,
            {ok, H};
        _ ->
            {ng, not_exists}
    end.

-spec update_member(_) -> {aborted, _} | {atomic, _}.
update_member(M) ->
    mnesia:transaction(fun() ->mnesia:write(M) end).

-spec generate_member_token(binary()) -> [integer()].
generate_member_token(Mail) ->
    generate_member_token(Mail, erlang:localtime()).

-spec generate_member_token(binary(), _) -> [integer()]. 
generate_member_token(Mail, Seed) ->
    Md5_bin = erlang:md5(lists:concat([binary_to_list(Mail), ";", tuple_to_list_deeply(Seed)])),
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= Md5_bin])).

-spec list_to_hex([byte()]) -> [[integer(),...]].
list_to_hex(L) ->    
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).
                      
tuple_to_list_deeply([]) -> [];
tuple_to_list_deeply(T) when is_tuple(T) ->
    tuple_to_list_deeply(tuple_to_list(T));
tuple_to_list_deeply([H|L]) ->
    lists:concat([tuple_to_list_deeply(H), tuple_to_list_deeply(L)]);
tuple_to_list_deeply(T) ->
    T.
