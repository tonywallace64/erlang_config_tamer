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
    "Hello" = substitute({macro,name,String},[{name,"Hello"}]),
    ["Hello","Tony"] = 
	substitute([
		    {macro,hello,String},
		    {macro,tony,String}],
		   [{hello,"Hello"},{tony,"Tony"}]),
    {some_value,"Hello","Tony"} = 
	substitute({some_value,
		    {macro,hello,String},
		    {macro,tony,String}},
		   [{hello,"Hello"},{tony,"Tony"}]).
    



    
    
