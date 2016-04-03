-module (hello).

-export([main/1]).

main(_) ->
    Name = get_name(),
    io:format("Hello ~s~n",[Name]).

get_name() ->
    {ok,Bin} = file:read_file("hello.conf.etf"),
    PL = binary_to_term(Bin),
    proplists:get_value(name,PL).
