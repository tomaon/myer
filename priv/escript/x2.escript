#!/usr/bin/env escript
%% -*- erlang -*-
%%! -config priv/conf/n1 -s crypto

%% -- myer --
run(9, H, undefined, myer) ->
    io:format("myer: ~p~n", [sys:get_state(element(3,H))]),
    ok;

run(8, H, [N,_], myer) ->
    {T, {ok,R}} = timer:tc(myer, unprepare, [H,N]),
    io:format("myer: unprepare=~p,~p~n", [R,T]),
    io:format("myer:           ~p~n", [sys:get_state(element(3,H))]),
    run(9, H, undefined, myer);

run(2, H, A, myer) ->
    {T, {ok,R}} = timer:tc(myer, prepare, [H|A]),
    io:format("myer: prepare=~p,~p~n", [R,T]),
    io:format("myer:         ~p~n", [sys:get_state(element(3,H))]),
    run(8, H, A, myer);

run(p, H, A, myer) ->
    profiling = eprof:start_profiling([element(3,H)]),
    run(2, H, A, myer),
    profiling_stopped = eprof:stop_profiling(),
    ok = eprof:analyze();
run(1, undefined, A, myer) ->
    case myer:checkout(mysql_pool) of
        {ok, H} ->
            run(2, H, A, myer),
            ok = myer:checkin(H)
    end;
%% -- --
run(0, undefined, undefined, A) ->
    case application:start(A) of
        ok ->
            L = [
                 <<"SELECT ?">>
                 %%<<"SELECT * FROM data_types_11_2_1 WHERE k > ?">>
                 %%<<"CALL x2(?)">>
                 %%<<"CALL x34(?)">>
                ],
            [ run(1, undefined, [<<"x2">>,E], A) || E <- L ],
            ok = application:stop(A)
    end.

main(_) ->
    L = [
         myer
        ],
    [ run(0, undefined, undefined, E) || E <- L ].
