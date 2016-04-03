%% throw away
-module(exceptions).

-export ([main/1]).

main([_Param]) ->
    F1 = fun() ->
		 receive X ->
			 generate_exception(X)
		 end
	 end,
    process_flag(trap_exit,true),
    XList = [1,2,3,4,5],
    PList = [{spawn_link(F1),X} || X <- XList],
    [Pid ! X || {Pid,X} <- PList],
    [receive Any -> Any end || X <- PList].

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT',a};
generate_exception(5) -> error(a).

