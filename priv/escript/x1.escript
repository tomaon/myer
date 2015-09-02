#!/usr/bin/env escript
%% -*- erlang -*-
%%! -config priv/conf/n1 -s crypto -s eprof

%% -- myer --
run(2, H, myer) ->
    F = fun (E) ->
                Q = <<"SELECT * FROM ", E/binary, " WHERE k > 0">>,
                [E, element(1,timer:tc(myer,real_query,[H,Q]))]
        end,
    [ io:format("myer: ~p=~p~n", F(E)) || E <- tables() ];
run(p, H, myer) ->
    profiling = eprof:start_profiling([element(3,H)]),
%   run(2, H, myer),
    timer:sleep(3000),
    profiling_stopped = eprof:stop_profiling(),
    ok = eprof:analyze();
run(1, undefined, myer) ->
    case myer:checkout(mysql_pool) of
        {ok, H} ->
            run(p, H, myer),
            ok = myer:checkin(H)
    end;
%% -- emysql --
run(2, undefined, emysql) ->
    F = fun (E) ->
                Q = <<"SELECT * FROM ", E/binary, " WHERE k > 0">>,
                [E, element(1,timer:tc(emysql,execute,[mysql_pool,Q]))]
        end,
    [ io:format("emysql: ~p=~p~n", F(E)) || E <- tables() ];
run(1, undefined, emysql) ->
    emysql:add_pool(mysql_pool, 1, "test", "test", "localhost", 3306, "test", utf8),
    run(2, undefined, emysql);
%% -- --
run(0, undefined, A) ->
    case application:start(A) of
        ok ->
            run(1, undefined, A),
            ok = application:stop(A)
    end.

main(_) ->
    L = [
%         emysql,
%         myer,
%         emysql,
%         myer,
%         emysql,
         myer
        ],
    [ run(0, undefined, E) || E <- L ].

tables() ->
    [
     <<"data_types_11_2_1">>, % int
     <<"data_types_11_2_2">>, % decimal
     <<"data_types_11_2_3">>, % float
%    <<"data_types_11_2_4">>, % bit
     <<"data_types_11_3_1">>, % date
     <<"data_types_11_3_2">>, % time
     <<"data_types_11_3_3">>, % year
     <<"data_types_11_4_1">>, % char
     <<"data_types_11_4_2">>, % binary
     <<"data_types_11_4_3">>, % blob
     <<"data_types_11_4_4">>, % enum
     <<"data_types_11_4_5">>  % set
    ].
