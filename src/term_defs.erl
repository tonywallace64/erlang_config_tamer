
-module(term_defs).

-export ([validate/2,test/0]).

-author('Tony Wallace').
-purpose(  <<"Confirm that an erlang term matches a specification.",
	   "The atom datadef matches any data definition",
	   "Type testing is done by matching the erlang term to {builtin,functionanme}",
	   "functionname is a unary function exported from the erlang module, for example"
	   "{builtin,is_integer} will check that the matched term is an integer./n",
	   "Where an erlang term must match a given term the {value,Term} pattern is used.",
	   "The {value,Term} construction is valuable where there is a list of valid options",
           ", for example {options,[{value,option1},{value,option2}]}.  In this case the",
	   "term can match either option1 or option 2./n",
	   "A list is defined by the construction {list,DataDef}, where each item of the",
	   "list must conform the the specification in DataDef.  For example a list of"
	   "integers is defined as {list,{builtin,is_integer}}.\n",
	   "Tuples are matched to {tuple,[DataDef]}.  Each term contained within the tuple",
	   "must match its associated DataDef./n",
	   "Property lists are given special treatment.  A property list is defined its contents",
	   ".  The specification is:/n ",
	   "     {property_list,[ ",
	   "         {tuple, ",
	   "             {options,{value,opt},{value,reqd}}, ",
	   "             Key,Specification}]} ",
	     " opt - this key is optional, reqd this key is required",
	     "Key - is an erlang term, normally an atom\n ",
	     "Specification a datadef that that key value must satisfy.">>).


-modification(<< "{{2016-04-28},ajw,"
	       "Add macro definition.  A macro is or the form {macro,Name :: atom(),DataDef()} ",
    "A macro is defined in the config, and its DataDef must match the datadef ",
    "for that part of its definition.  The definition is substituted at runtime ",
    "and any variable substituted must match DataDef." >>).

validate([Def],Term) ->
    %% When consulting a file, this can add an extra list
    %% this removes it.
    validate(Def,Term);
validate(datadef,[Term]) ->
    validate(datadef,Term);
validate(Def,Term)  ->
    {R,_}=validate(Def,Term,dict:new()),
    R.
validate(Def,Term,State) ->
    %io:format("validate(Def=~p,Term=~p,~p)~n~n",[Def,Term,State]),
    case maybe_validate(Def,Term,State) of
	{true,NewState} ->
	    {true,NewState};
	{false,_} ->
	    not_valid(Def,Term,State)
    end.
%maybe_validate([Def],Term,State) ->
%    validate(Def,Term,State);
maybe_validate(DataDef,{macro,_Name,DataDef},State) ->
    {true,State};
maybe_validate({singleton,DataDef},[Value],State) ->
    validate(DataDef,Value,State);
maybe_validate({define,Key,Value},_Term,State)  ->
    NewState = dict:store(Key,Value,State),
    {true,NewState};
maybe_validate(term,_,S) ->
    {true,S};
maybe_validate({property_list,KeyDefs},PL,State) ->
    Checked = [pl_entry(KeyDef,PL,State) || KeyDef <- KeyDefs],
    R=lists:foldl(fun(true,A) -> A;(_,_)->false end, true, Checked),
    {R,State};
maybe_validate({tuple,DefList},Term,S) 
  when is_tuple(Term) andalso (length(DefList) =:= tuple_size(Term)) ->
    TermList = tuple_to_list(Term),
    tuple_flds(DefList,TermList,S);
maybe_validate({tuple,_},_,S) ->
    %% tuple sizes do not match or Term is not a tuple
    {false,S};
maybe_validate({list,TermDef},Term,State) 
  when is_list(Term)  ->
    Valid=[validate(TermDef,X,State) || X <- Term],
    R=lists:foldl(fun({X,_},A) -> A and X end,true,Valid),
    {R,State};
maybe_validate(TermDef={builtin,Atom},Term,State)   ->
    case catch(apply(erlang,Atom,[Term])) of
	{'EXIT',_} ->
	    not_valid(TermDef,'undefined',State);
	true -> {true,State};
	false -> {false,State};
	X -> 
	    not_valid(TermDef,{'not_boolean',X})
    end;
maybe_validate({options,TermDef},Term,State) when is_list(TermDef) ->
    choices(TermDef,Term,State);
maybe_validate({match_all,[]},_,State)  ->
    {true,State};
maybe_validate({match_all,[H|T]},Term,State)  ->
    {R1,State1} = validate(H,Term,State),
    case R1 of
	true ->
	    validate({match_all,T},Term,State1);
	false ->
	    not_valid(H,Term,State)
    end;
maybe_validate({value,X},X,State) ->
    {true,State};
maybe_validate(datadef,datadef,State) ->
    {true,State};
maybe_validate(datadef,{singleton,Def},State) ->
    validate(datadef,Def,State);
maybe_validate(datadef,{match_all,DefList},State) ->
    S1=lists:foldl(fun(T,S) -> {true,NewState} = validate(datadef,T,S),NewState end,State,DefList),
    {true,S1};
maybe_validate(datadef,{define,Atom,DataDef},State) when is_atom(Atom) ->
    validate({define,Atom,DataDef},placeholder,State);
maybe_validate(datadef,term,State) ->
    {true,State};
maybe_validate(datadef,{value,_},State) ->
    {true,State};
maybe_validate(datadef,{builtin,Fname},State)
  when is_atom(Fname)->
    EE = erlang:module_info(exports),
    case proplists:get_value(Fname,EE) of
	1 -> {true,State};
	_ -> {false,State}
    end;
maybe_validate(datadef,{list,ItemDef},State) ->
    validate(datadef,ItemDef,State);
maybe_validate(datadef,{property_list,[{Opt,KeyName,DataDef}|KeyList]},State) 
  when ((Opt =:= reqd) or (Opt =:= opt)) and is_atom(KeyName) ->
    {true,NewState} = validate(datadef,DataDef,State),
    maybe_validate(datadef,{property_list,KeyList},NewState);
maybe_validate(datadef,{property_list,[]},S) -> {true,S};
maybe_validate(datadef,{tuple,DefList},State) 
  when is_list(DefList)->
    Validated = [validate(datadef,X,State) || X <- DefList],
    R=lists:foldl(fun({true,_},A) -> A;(_,_) -> false end,true,Validated),
    {R,State};
maybe_validate(datadef,Atom,State) when is_atom(Atom) ->
    case lookup(Atom,State) of
	undefined -> {false,State};
	_ -> {true,State}
    end;
maybe_validate(datadef,{options,[H|T]},State)  ->
    {true,NewState} = validate(datadef,H,State),
    validate(datadef,{options,T},NewState);
maybe_validate(datadef,{options,[]},State) ->
    {true,State};
maybe_validate(Key,Term,State) when is_atom(Key) ->
    case lookup(Key,State) of
	undefined -> {false,State};
	Pattern -> validate(Pattern,Term,State)
    end;
maybe_validate(_,_,State) ->
    {false,State}.

lookup(Key,Dict) ->
    key_exists_lookup(dict:is_key(Key,Dict),Key,Dict).
key_exists_lookup(false,_,_) ->
    undefined;
key_exists_lookup(true,Key,Dict) ->
    dict:fetch(Key,Dict).

pl_entry({Opt,Key,Def},PL,State) ->
    case proplists:get_value(Key,PL) of
	undefined ->
	    %% it is valid for an optional key to be undefined
	    (Opt =:= opt);
	Data ->
	    %% if it exists it must be valid
	    {true,_}=validate(Def,Data,State),
	    true
    end.

tuple_flds([H1|T1],[H2|T2],State) ->
    case validate(H1,H2,State) of
	{true,NewState} ->
	    tuple_flds(T1,T2,NewState);
	{false,_S} ->
	    not_valid(H1,H2,State)
    end;
    

tuple_flds([],[],State) ->
    {true,State}.

choices([H|T],Term,State) ->
    %io:format("choices ([~p|~p],~p,~p)~n",[H,T,Term,State]),
    case (catch maybe_validate(H,Term,State)) of
	{'EXIT',_} ->
	    choices(T,Term,State);
	{invalid,_,_,_} ->
	    choices(T,Term,State);
	{true,NewState} -> 
	    {true,NewState};
	{false,NewState} -> 
	    choices(T,Term,NewState)
    end;

choices([],_,State) -> 
    {false,State}.


not_valid(Def,Term) ->
    throw({invalid,Def,Term}).
    
not_valid(Def,Term,State) ->
    throw({invalid,Def,Term,State}).
    
   
test() ->
    T=test([
	    {datadef,datadef,true},
	    {datadef,term,true},
	    {datadef,{builtin,is_integer},true},
	    {datadef,{value,value},true},
	    {datadef,{tuple,[{value,employee}]},true},
	    {datadef,{tuple,employee},invalid},
	    {datadef,{list,{builtin,is_integer}},true},
	    {datadef,{property_list,[]},true},
	    {datadef,{property_list,[{reqd,mhs_server,{builtin,is_atom}}]},true},
	    {datadef,{match_all,[{define,int,{builtin,is_integer}},int]},true},
	    {datadef,{options,[{value,male},{builtin,is_integer}]},true},
	    {{builtin,is_integer},5,true},
	    {{builtin,is_integer},{macro,age,{builtin,is_integer}},true},
	    {{builtin,is_integer},{macro,sex,{options,[{value,male},{value,female}]}},invalid},
	    {{builtin,is_integer},atom,invalid},
	    {{options,[{value,option1},{value,option2}]},option2,true},
	    {{options,[{value,option1},{value,option2}]},option3,invalid},
	    {{options,[{tuple,[{value,rec1}]},{tuple,[{value,rec2}]}]},{rec2},true},
	    {{options,[{tuple,[{value,rec1}]},{tuple,[{value,rec2}]}]},
	       {macro,arec,{tuple,[{value,rec2}]}},true},
	    {{tuple,[{value,employee},{builtin,is_list}]},{employee,"Robert"},true},
	    {{tuple,[{value,employee},{builtin,is_list}]},{emp,"Robert"},invalid},
	    {{tuple,[{value,employee},{builtin,is_list}]},{employee,"Robert",male},invalid},
	    {{list,{builtin,is_integer}},[3,4,5],true},
	    {{list,term},[3,a,5,"Hello",{some_tuple}],true},
	    {{list,{builtin,is_integer}},[3,a,5],invalid},
	    {{property_list,[]},[],true},
	    {{property_list,[{opt,gender,{options,[{value,male},{value,female}]}}]},[],true},
	    {{property_list,[{opt,gender,{options,[{value,male},{value,female}]}}]},[{gender,male}],true},
	    {{property_list,[{opt,gender,{options,[{value,male},{value,female}]}}]},[{gender,transgender}],invalid},
	    {{property_list,[{reqd,gender,{options,[{value,male},{value,female}]}}]},[],invalid},
	    {{match_all,[{define,int,{builtin,is_integer}},int]},5,true},
	    {{match_all,[{define,int,{builtin,is_integer}},int]},'hello',invalid},
	    {{match_all,[
	      {define,iolistmember,{options,[{builtin,is_integer},iolist]}},
	      {define,iolist,{options,[{builtin,is_binary},{list,iolistmember}]}},
	      iolist]},
	     <<"This is a valid iolist">>,true},
	    {{match_all,[
	      {define,iolistmember,{options,[{builtin,is_integer},iolist]}},
	      {define,iolist,{options,[{builtin,is_binary},{list,iolistmember}]}},
	      iolist]},
	      [<<"This is a valid ">>,"iolist"],true},
	    {{match_all,[
	      {define,iolistmember,{options,[{builtin,is_integer},iolist]}},
	      {define,iolist,{options,[{builtin,is_binary},{list,iolistmember}]}},
	      iolist]},
	      'This is not a valid iolist',
	       invalid},
	    {{match_all,[
	      {define,iolistmember,{options,[{builtin,is_integer},iolist]}},
	      {define,iolist,{options,[{builtin,is_binary},{list,iolistmember}]}},
	      iolist]},
	      "This is a valid iolist",true}
	  ]),
    Filtered=[{X,Y} || {X,Y} <- T, Y =/= pass],
    io:format("~p~n",[Filtered]).
test(L) when is_list(L) ->
    [do_one_test(H) || H <- L].

do_one_test(T={Def,Arg,Res}) ->
    PassFail = mustbe(Res,R2=(catch validate(Def,Arg))),
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
