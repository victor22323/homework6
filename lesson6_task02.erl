-module(lesson6_task02).
-compile(export_all).



init_per_suite(Config) ->
    application:start(homework6),
    Config.

end_per_suite(_Config) ->

    application:stop(homework6).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
   
    [create_table_test, insert_test, lookup_test, expiration_test].

create_table_test(Config) ->
    TableName = test_table,
    ok = homework6:create(TableName),
    ?assertMatch(true, ets:info(TableName) /= undefined),
    Config.

insert_test(Config) ->
    TableName = test_table,
    Key = test_key,
    Value = test_value,
    ok = homework6:insert(TableName, Key, Value),
    ?assertEqual([{Key, Value, infinity}], ets:lookup(TableName, Key)),
    Config.


lookup_test(Config) ->
    TableName = test_table,
    Key = test_key,
    Value = test_value,
    Result = homework6:lookup(TableName, Key),
    ?assertEqual(Value, Result),
    Config.


expiration_test(Config) ->
    TableName = test_table,
    Key = expiring_key,
    Value = expiring_value,
    TTL = 1,
    ok = homework6:insert(TableName, Key, Value, TTL),
    timer:sleep(2000), 
    Result = homework6:lookup(TableName, Key),
    ?assertEqual(undefined, Result),
    Config.
