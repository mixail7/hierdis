-module(hierdis_tests).

-include_lib("eunit/include/eunit.hrl").

get_set_test() ->
    C = c(),
    ?assertMatch({ok, _}, hierdis:command(C, ["DEL", "foo"])),

    ?assertEqual({ok, undefined}, hierdis:command(C, ["GET", "foo"])),
    ?assertEqual({ok, <<"OK">>}, hierdis:command(C, ["SET", "foo", "bar"])),
    ?assertEqual({ok, <<"bar">>}, hierdis:command(C, ["GET", "foo"])).

c() ->
    Res = hierdis:connect("redis", 6379),
    ?assertMatch({ok, _}, Res),
    {ok, C} = Res,
    C.
