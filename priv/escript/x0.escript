#!/usr/bin/env escript
%% -*- erlang -*-
%%! -config priv/conf/n1 -s crypto

run(3, _, _, _) ->
    ok;
run(2, H, A, myer) ->
    L = [
         <<"DROP PROCEDURE IF EXISTS x2">>,
         <<"CREATE PROCEDURE x2 ("
           "  IN i_in INT"
           ")"
           "BEGIN"
           "  SELECT i_in * 2 AS value;"
           "END">>,
         <<"DROP PROCEDURE IF EXISTS x34">>,
         <<"CREATE PROCEDURE x34 ("
           "  IN i_in INT"
           ")"
           "BEGIN"
           "  SELECT i_in * 3 AS value;"
           "  SELECT i_in * 4 AS value;"
           "END">>
        ],
    [ {ok, _} = myer:real_query(H, E) || E <- L ],
    run(3, H, A, myer);
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
                 undefined
                ],
            [ run(1, undefined, E, A) || E <- L ],
            ok = application:stop(A)
    end.

main(_) ->
    L = [
         myer
        ],
    [ run(0, undefined, undefined, E) || E <- L ].
