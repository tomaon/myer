#!/usr/bin/env escript
%% -*- erlang -*-
%%! -config priv/conf/n1 -s crypto

-include_lib("myer/include/myer.hrl").

%% -- myer --
run(9, _, undefined, myer) ->
    ok;
run(8, H, P1, myer) ->
    {T, ok} = timer:tc(myer, stmt_close, [H,P1]),
    io:format("myer: stmt_close=~p~n", [T]),
    run(9, H, undefined, myer);
run(7, H, P1, myer) ->
    {T, {ok,P2}} = timer:tc(myer, stmt_reset, [H,P1]),
    io:format("myer: stmt_reset=~p~n", [T]),
    run(8, H, P2, myer);

run(6, H, P1, myer) ->
    {T, {ok,P2}} = timer:tc(myer, stmt_fetch, [H,P1]),
    P2 = P1,
    io:format("myer: stmt_fetch=~p~n", [T]),
    run(7, H, P2, myer);

%% stmt_next_result

run(3, H, P1, myer) ->
    {T, V} = timer:tc(myer, stmt_execute, [H,P1,[0]]),
    case V of
        {ok,R,P2} ->
            io:format("myer: stmt_execute=~p~n", [T]),
            12 = element(3, P2), % field_count
            25 = length(R),
            N = case myer:more_results(P2) of true -> 5; false -> 7 end,
            run(N, H, P2, myer)
    end;
run(2, H, undefined, myer) ->
    Q = <<"SELECT * FROM data_types_11_2_1 WHERE k > ?">>,
    {T, {ok,P2}} = timer:tc(myer, stmt_prepare, [H,Q]),
    12 = myer:stmt_field_count(P2),
    1  = myer:stmt_param_count(P2),
    io:format("myer: stmt_prepare=~p~n", [T]),
    run(3, H, P2, myer);
run(p, H, undefined, myer) ->
    profiling = eprof:start_profiling([element(3,H)]),
    run(2, H, undefined, myer),
    profiling_stopped = eprof:stop_profiling(),
    ok = eprof:analyze();
run(1, undefined, undefined, myer) ->
    case myer:checkout(mysql_pool) of
        {ok, H} ->
            run(2, H, undefined, myer),
            ok = myer:checkin(H)
    end;
%% -- --
run(0, undefined, undefined, A) ->
    case application:start(A) of
        ok ->
            run(1, undefined, undefined, A),
            ok = application:stop(A)
    end.

main(_) ->
    L = [
%         myer,
%         myer,
         myer
        ],
    [ run(0, undefined, undefined, E) || E <- L ].
