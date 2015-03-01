-record(message, {message_id :: [integer()], content :: binary(), user :: binary(), at}).
-type message()::#message{}.

-record(member, {mail :: binary(), password :: binary(), name :: binary()}).
-type member()::#member{}.

-record(session, {member :: member(), md5 :: binary(), token :: [integer()]}).
-type session()::#session{}.

-define(record_to_struct(RecordName, Record),
  {lists:zip(
      lists:map(fun(F) ->
          list_to_binary(atom_to_list(F)) end, record_info(fields, RecordName)),
      lists:map(
          fun(undefined) ->     
              null;
          (E) when is_binary(E) -> 
              list_to_atom(binary_to_list(E));
          (E) when is_list(E) ->
              list_to_atom(E);
          (E) -> E
          end,
        tl(tuple_to_list(Record))
      )
    )
  }
).
