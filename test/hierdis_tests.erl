-module(hierdis_tests).

-include_lib("eunit/include/eunit.hrl").

get_set_test() ->
    C = c(),
    ?assertMatch({ok, _}, hierdis:command(C, ["DEL", "foo"])),

    ?assertEqual({ok, undefined}, hierdis:command(C, ["GET", "foo"])),
    ?assertEqual({ok, <<"OK">>}, hierdis:command(C, ["SET", "foo", "bar"])),
    ?assertEqual({ok, <<"bar">>}, hierdis:command(C, ["GET", "foo"])),
    ?assertEqual({ok, <<"bar">>}, hierdis:command(C, ["GET", "foo"], 5000)),
    ?assertError(badarg, hierdis:command(C, ["GET", "foo"], -1)),
    ?assertError(badarg, hierdis:command(C, ["GET", "foo"], bar)),
    ?assertError(badarg, hierdis:command(C, ["GET", foo])).

delete_test() ->
    C = c(),
    ?assertMatch({ok, _}, hierdis:command(C, ["DEL", "foo"])),

    ?assertEqual({ok, <<"OK">>}, hierdis:command(C, ["SET", "foo", "bar"])),
    ?assertEqual({ok, 1}, hierdis:command(C, ["DEL", "foo"])),
    ?assertEqual({ok, undefined}, hierdis:command(C, ["GET", "foo"])).

mset_mget_test() ->
    C = c(),
    Keys = lists:seq(1, 10),
    ListKeys = [integer_to_list(K) || K <- Keys],

    ?assertMatch({ok, _}, hierdis:command(C, ["DEL" | ListKeys])),

    KeyValuePairs = [[integer_to_list(K), integer_to_list(K*2)] || K <- Keys],
    ExpectedResult = [list_to_binary(integer_to_list(K*2)) || K <- Keys],

    ?assertEqual({ok, <<"OK">>}, hierdis:command(C, ["MSET" | lists:append(KeyValuePairs)])),
    ?assertEqual({ok, ExpectedResult}, hierdis:command(C, ["MGET" | ListKeys])),
    ?assertMatch({ok, _}, hierdis:command(C, ["DEL" | ListKeys])).

transaction_test() ->
    C = c(),

    ?assertMatch({ok, _}, hierdis:command(C, ["LPUSH", "k1", "b"])),
    ?assertMatch({ok, _}, hierdis:command(C, ["LPUSH", "k1", "a"])),
    ?assertMatch({ok, _}, hierdis:command(C, ["LPUSH", "k2", "c"])),

    ExpectedResult = [[<<"a">>, <<"b">>], [<<"c">>]],

    ?assertEqual({ok, ExpectedResult}, hierdis:transaction(C, [["LRANGE", "k1", "0", "-1"], ["LRANGE", "k2", "0", "-1"]])),

    ?assertMatch({ok, _}, hierdis:command(C, ["DEL", "k1", "k2"])).

transaction_nil_test() ->
    C1 = c(),
    C2 = c(),

    ?assertEqual({ok, <<"OK">>}, hierdis:command(C1, ["WATCH", "x"])),
    ?assertMatch({ok, _}, hierdis:command(C2, ["INCR", "x"])),
    ?assertEqual({ok, undefined}, hierdis:transaction(C1, [["GET", "x"]], 5000)),
    ?assertMatch({ok, _}, hierdis:command(C1, ["DEL", "x"])).

pipeline_test() ->
    C = c(),

    P1 = [["SET", "a", "1"],
          ["LPUSH", "b", "3"],
          ["LPUSH", "b", "2"]],

    ?assertEqual([{ok, <<"OK">>}, {ok, 1}, {ok, 2}],
                 hierdis:pipeline(C, P1)),

    P2 = [["MULTI"],
          ["GET", "a"],
          ["LRANGE", "b", "0", "-1"],
          ["EXEC"]],

    ?assertEqual([{ok, <<"OK">>},
                  {ok, <<"QUEUED">>},
                  {ok, <<"QUEUED">>},
                  {ok, [<<"1">>, [<<"2">>, <<"3">>]]}],
                 hierdis:pipeline(C, P2, 5000)),

    ?assertMatch({ok, _}, hierdis:command(C, ["DEL", "a", "b"])).

pipeline_mixed_test() ->
    C = c(),
    P1 = [["LPUSH", "c", "1"] || _ <- lists:seq(1, 100)],
    P2 = [["LPUSH", "d", "1"] || _ <- lists:seq(1, 100)],
    Expect = [{ok, I} || I <- lists:seq(1, 100)],
    spawn(fun () ->
                  erlang:yield(),
                  ?assertEqual(Expect, hierdis:pipeline(C, P1))
          end),
    spawn(fun () ->
                  ?assertEqual(Expect, hierdis:pipeline(C, P2))
          end),
    timer:sleep(10),
    ?assertMatch({ok, _}, hierdis:command(C, ["DEL", "c", "d"])).

c() ->
    Res = hierdis:connect("redis", 6379),
    ?assertMatch({ok, _}, Res),
    {ok, C} = Res,
    C.
