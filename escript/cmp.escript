#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/emysql/ebin deps/poolboy/ebin -s crypto -config files/shell

pre(1) ->
    myer:start();
pre(0) ->
    application:start(emysql),
    emysql:add_pool(mysql_pool, 1, "test", "test", "localhost", 20506, "test", utf8);
pre(_) ->
    ok.

post(1) ->
    myer:stop();
post(0) ->
    application:stop(emysql);
post(_) ->
    ok.

run(1, Q) ->
    _Result = myer_app:call(mysql_pool, {real_query,[Q]}),
    %%io:format("result=~p~n", [_Result]),
    ok;
run(0, Q) ->
    _Result = emysql:execute(mysql_pool, Q),
    %%io:format("result: ~p~n", [_Result]),
    ok;
run(_,_) ->
    ok.

run(N) ->
    L = [
         <<"data_types_11_2_1">>, % int
         <<"data_types_11_2_2">>, % decimal
         <<"data_types_11_2_3">>, % float
         %%<<"data_types_11_2_4">>, % bit
         <<"data_types_11_3_1">>, % date
         <<"data_types_11_3_2">>, % time
         <<"data_types_11_3_3">>, % year
         <<"data_types_11_4_1">>, % char
         <<"data_types_11_4_2">>, % binary
         <<"data_types_11_4_3">>, % blob
         <<"data_types_11_4_4">>, % enum
         <<"data_types_11_4_5">>  % set
        ],

    pre(N),
    F = fun (E) ->
                Q = <<"SELECT * FROM ", E/binary, " WHERE k > 0">>,
                {T, _} = timer:tc(fun() -> [ run(N,Q) || _ <- lists:seq(1,100) ] end),
                io:format("n=~p, e=~p, t=~p~n", [N,E,T])
        end,
    [ F(E) || E <- L],
    post(N),
    io:format("~n").

main(_) ->
    L = [ 0, 1, 0, 1, 0, 1 ],
    [ run(E) || E <- L ].
