-module (config_check).
-export([main/1,test/0]).

-purpose (
<<"Write etf version of Config file if valid.",
  "Other requirements are: runable as escript">>).

main(["test"]) ->
    test();
main([ConfigFile,SchemaFile]) ->
    [DataDef]    = get_valid_schema(SchemaFile),
    {ok,Config}  = maybe_consult(ConfigFile),
    io:format("datadef ~p~n",[DataDef]),
    io:format("config ~p~n",[Config]),
    Result       = term_defs:validate(DataDef,Config),
    write_results(Result,ConfigFile,Config).

write_results(true,ConfigFile,Config) ->
    OutputFile = [ConfigFile,".etf"],
    EtfData = term_to_binary([Config],[]),
    ok = file:write_file(OutputFile,EtfData);

write_results(false,_,_) ->
    ok.

get_valid_schema(SchemaFile) ->
    Ext = filename:extension(SchemaFile),
    get_valid_schema_by_ext(Ext,SchemaFile).

get_valid_schema_by_ext(".etf",SchemaFile) ->
    %% a compiled schema must be valid
    get_term_by_ext(".etf",SchemaFile);
get_valid_schema_by_ext(Ext,SchemaFile) ->
    %% a schema term must be a valid datadef
    %% checking this now is an example of fail fast
    Def = get_term_by_ext(Ext,SchemaFile),
    true = term_defs:validate(datadef,Def),
    Def.

get_term_by_ext(".etf",SchemaFile) ->
    {ok,Etf} = file:read_file(SchemaFile),
    binary_to_term(Etf);
get_term_by_ext(_,SchemaFile) ->
    {ok,DataDef} = maybe_consult(SchemaFile),
    DataDef.

maybe_consult(Filename) ->    
    test_consult(file:consult(Filename),Filename).
test_consult({ok,Value},_) -> 
    {ok,Value};
test_consult({error,Reason},Filename) ->
    throw({error,{Reason,Filename}}).

test() ->
    DataDef = "{singleton,{value,valid}}.",
    DefFile = "main_test.config_def",
    ok = file:write_file(DefFile,DataDef),
    ok = file:write_file("main_test_valid","valid."),
    ok = file:write_file("main_test_invalid","invalid."),
    main(["main_test_valid",DefFile]),
    %% correct behaviour of second test is to throw an exception
    %% catch it and clean up
    catch main(["main_test_invalid",DefFile]),
    true = filelib:is_regular("main_test_valid.etf"),
    false = filelib:is_regular("main_test_invalid.etf"),
    file:delete(DefFile),
    file:delete("main_test_valid"),
    file:delete("main_test_invalid"),
    file:delete("main_test_valid.etf"),
    pass.
    
