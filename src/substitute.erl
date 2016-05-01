-module (substitute).

-export([substitute/2,test/0]).

-type proplist()::[{any(),any()}].
-spec substitute(Data,SubstList) -> OutData when
      Data :: any(),
      SubstList :: proplist(),
      OutData :: any().

substitute(X, SL) when is_list(X) ->
    list_sub(X ,[],SL);
substitute({macro,Name,Def},SL) ->
    macro_sub(Name,Def,SL);
substitute(X, SL) when is_tuple(X) ->
    L = tuple_to_list(X),
    NL = list_sub(L, [], SL),
    list_to_tuple(NL);
substitute(X,_) ->
    X.


list_sub([], Acc, _) ->
    lists:reverse(Acc);
list_sub([H|T], Acc, SL) ->
    list_sub(T,[substitute(H,SL)|Acc],SL).

macro_sub(Name,Def,SL) ->
    {_,V}=proplists:lookup(Name,SL),
    true = term_defs:validate(Def,V),
    V.

test() ->
    String = {list,{builtin,is_integer}},
    test(fun substitute/2, 
	 [{"Hello",[{macro,name,String},[{name,"Hello"}]],true},
	  {"Hello",[{macro,name,{builtin,is_integer}},[{name,"Hello"}]],invalid},
	  {["Hello","Tony"],
	   [[
		    {macro,hello,String},
		    {macro,tony,String}],
		   [{hello,"Hello"},{tony,"Tony"}]],true},
	  {{some_value,"Hello","Tony"}, 
	   [{some_value,
		    {macro,hello,String},
		    {macro,tony,String}},
		   [{hello,"Hello"},{tony,"Tony"}]],true}]).
    

test(F,L) when is_list(L) ->
    [do_one_test(F,H) || H <- L].

do_one_test(F,T={Def,[Arg1,Arg2],Res}) ->
    PassFail = mustbe(Res,R2=(catch Def =:= F(Arg1,Arg2))),
    mr(PassFail,T,R2).

mustbe(invalid,{invalid,_,_,_}) ->
    pass;
mustbe(invalid,{'EXIT',_}) ->
    pass;
mustbe(true,true) ->
    pass;
mustbe(A,B) ->
    io:format("Expected Resut is ~p~n actual result is ~p~n failed~n~n",[A,B]),
    fail.

mr(pass,T,_) ->
    {T,pass};
mr(fail,T,R) ->
    {T,R}.



    
    
