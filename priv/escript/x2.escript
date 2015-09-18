#!/usr/bin/env escript
%% -*- erlang -*-
%%! -config priv/conf/n1 -s crypto

next(H,T,F) -> case myer:more_results(H) of true -> T; false -> F end.

%% -- myer --
run(6, H, undefined, myer) ->
    io:format("myer: 6~n"),
    io:format("myer: ~p~n", [sys:get_state(element(3,H))]),
    ok;
run(5, H, N, myer) ->
    io:format("myer: 5~n"),
    {T, {ok,R}} = timer:tc(myer, unprepare, [H,N]),
    io:format("myer: unprepare=~p,~p:~p~n", [T,R,myer_debug:stat(R)]),
    io:format("myer:           ~p~n", [sys:get_state(element(3,H))]),
    run(6, H, undefined, myer);
run(4, H, N, myer) ->
    io:format("myer: 4~n"),
    case timer:tc(myer, next_result, [H]) of
        {T, {ok,R}} ->
            io:format("myer: next_result=~p,~p:~p~n", [T,R,myer_debug:stat(R)]),
            io:format("myer:             ~p~n", [sys:get_state(element(3,H))]),
            run(next(R,4,5), H, N, myer);
        {T, {ok,F,D,R}} ->
            io:format("myer: next_result=~p,~p:~p~n", [T,R,myer_debug:stat(R)]),
            io:format("myer:             ~p~n", [F]),
            io:format("myer:             ~p~n", [D]),
            io:format("myer:             ~p~n", [sys:get_state(element(3,H))]),
            run(next(R,4,5), H, N, myer)
    end;
run(3, H, N, myer) ->
    io:format("myer: 3~n"),
    case timer:tc(myer, execute, [H,N,[1]]) of
        {T, {ok,R}} ->
            io:format("myer: execute=~p,~p:~p~n", [T,R,myer_debug:stat(R)]),
            io:format("myer:         ~p~n", [sys:get_state(element(3,H))]),
            run(next(R,4,5), H, N, myer);
        {T, {ok,F,D,R}} ->
            io:format("myer: execute=~p,~p:~p~n", [T,R,myer_debug:stat(R)]),
            io:format("myer:         ~p~n", [F]),
            io:format("myer:         ~p~n", [D]),
            io:format("myer:         ~p~n", [sys:get_state(element(3,H))]),
            run(next(R,4,5), H, N, myer)
    end;
run(2, H, [N,Q], myer) ->
    io:format("myer: 2~n"),
    {T, {ok,R}} = timer:tc(myer, prepare, [H,N,Q]),
    io:format("myer: prepare=~p,~p:~p~n", [T,R,myer_debug:stat(R)]),
    io:format("myer:         ~p~n", [sys:get_state(element(3,H))]),
    run(3, H, N, myer);
run(p, H, A, myer) ->
    profiling = eprof:start_profiling([element(3,H)]),
    run(2, H, A, myer),
    profiling_stopped = eprof:stop_profiling(),
    ok = eprof:analyze();
run(1, undefined, A, myer) ->
    io:format("myer: 1~n"),
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
                 <<"SELECT ?">>,
                 <<"SELECT * FROM data_types_11_2_1 WHERE k > ?">>,
                 <<"CALL x2(?)">>, % begin select i_in * 2 as value; end
                 <<"CALL x34(?)">> % begin select i_in * 3 as value; select i_in * 4 as value; end
                ],
            [ run(1, undefined, [<<"x2">>,E], A) || E <- L ],
            ok = application:stop(A)
    end.

main(_) ->
    L = [
         myer
        ],
    [ run(0, undefined, undefined, E) || E <- L ].
