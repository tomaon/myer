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

%% stmt_fetch more_results stmt_next_result, ...

run(3, H, P1, myer) ->
    {T, V} = timer:tc(myer, stmt_execute, [H,P1,[0]]),
    case V of
        {ok,F,R,P2} ->
            io:format("myer: stmt_execute=~p, r=~p/f=~p~n", [T,length(R),length(F)]),
            run(7, H, P2, myer)
    end;
run(2, H, undefined, myer) ->
    Q = <<"SELECT * FROM data_types_11_2_1 WHERE k > ?">>,
    {T, {ok,P2}} = timer:tc(myer, stmt_prepare, [H,Q]),
    io:format("myer: stmt_prepare=~p~n", [T]),
    run(3, H, P2, myer);
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
         myer
        ],
    [ run(0, undefined, undefined, E) || E <- L ].
