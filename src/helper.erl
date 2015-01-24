-module(helper).

-compile(export_all).

-spec get_request_prefix() -> any().
get_request_prefix() ->
    PrefixTmp = econfig:get_value(onshin, "server", "prefix"),
        case PrefixTmp of 
            undefined ->
                Prefix = "";
            _ -> Prefix = PrefixTmp
    end,
    lager:info("Prefix ~p", [Prefix]),
    Prefix.
