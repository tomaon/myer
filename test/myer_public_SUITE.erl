%% =============================================================================
%% Copyright 2013 Tomohiko Aono
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% =============================================================================

-module(myer_public_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("myer/include/myer.hrl").

-define(TABLE, ?MODULE_STRING).

all() -> [
          {group, test_normal},
          {group, test_compress}
         ].

groups() ->
    [
     {test_normal,   [], [
                          {group, test_v56_pool},
                          {group, test_v55_pool},
                          {group, test_v51_pool},
                          {group, test_v50_pool},
                          {group, test_v41_pool},
                          {group, test_v40_pool}
                         ]},
     {test_compress, [], [
                          {group, test_v56_pool},
                          {group, test_v55_pool},
                          {group, test_v51_pool},
                          {group, test_v50_pool},
                          {group, test_v41_pool},
                          {group, test_v40_pool}
                         ]},

     {test_v56_pool, [], [{group,query_test},{group,blob_test},{group,other_test}]},
     {test_v55_pool, [], [{group,query_test},{group,other_test}]},
     {test_v51_pool, [], [{group,query_test},{group,other_test}]},
     {test_v50_pool, [], [{group,other_test}]},
     {test_v41_pool, [], [{group,other_test}]},
     {test_v40_pool, [], [{group,other_test},{group,cover_test}]},

     {query_test, [], [
                       real_test_crud,
                       real_test_transaction,
                       real_test_multi,
                       real_test_call_1, real_test_call_2,

                       stmt_test_crud,
                       stmt_test_count_0_0, stmt_test_count_0_1,
                       stmt_test_count_1_1, stmt_test_count_3_0,
                       stmt_test_fetch,
                       stmt_test_call_1, stmt_test_call_1
                      ]},
     {blob_test,  [], [
                       stmt_test_blob
                      ]},
     {other_test, [], [
                       ping_test,
                       stat_test,
                       refresh_test,
                       select_db_test
                      ]},
     {cover_test, [], [
                       cover_myer,
                       cover_myer_app,
                       cover_myer_client
                      ]}
    ].

init_per_suite(Config) ->
    _ = application:load(myer),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(Group, Config) ->
    case list_to_binary(lists:reverse(atom_to_list(Group))) of
        <<"loop_",_/binary>> ->
            L = [fun set_env/1, fun start/1 ],
            lists:foldl(fun(E,A) -> E(A) end, [{pool,Group}|Config], L);
        <<"lamron_",_/binary>> ->
            [{compress,false}|Config];
        <<"sserpmoc_",_/binary>> ->
            [{compress,true}|Config];
        _ ->
            Config
    end.

end_per_group(Group, Config) ->
    case list_to_binary(lists:reverse(atom_to_list(Group))) of
        <<"loop_",_/binary>> ->
            L = [fun stop/1 ],
            lists:foldl(fun(E,A) -> E(A) end, proplists:delete(pool,Config), L);
        <<"lamron_",_/binary>> ->
            proplists:delete(compress, Config);
        <<"sserpmoc_",_/binary>> ->
            proplists:delete(compress, Config);
        _ ->
            Config
    end.

init_per_testcase(_TestCase, Config) -> % use erlang:monitor/2 (poolboy:checkout/3)
    L = [ fun connect/1, fun get_server_version/1, fun drop_table/1, fun testcase_pre/1 ],
    lists:foldl(fun(E,A) -> E(A) end, Config, L).

end_per_testcase(_TestCase, Config) ->
    L = [ fun testcase_post/1, fun drop_table/1, fun close/1, fun cleanup/1 ],
    lists:foldl(fun(E,A) -> E(A) end, Config, L).

%% == group: query_test ==

real_test_crud(Config) ->
    real_test_crud(Config, ?config(version,Config) > [5,1,0]).

real_test_crud(_Config, false) ->
    {skip, not_supported};
real_test_crud(Config, true) ->

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),
    begin % -- Create --
        {ok, C} = real_query(Config, <<"INSERT INTO " ?TABLE " VALUES (560,'mysql',+5.6)">>),
        1 = affected_rows(C), 0 = insert_id(C), 0 = warning_count(C)
    end,

    begin % -- Read --
        L = [ [560,<<"mysql">>,+5.6] ],
        {ok, _, L, R} = real_query(Config, <<"SELECT * FROM " ?TABLE " WHERE id = 560">>),
        undefined = affected_rows(R), 0 = warning_count(R)
    end,

    begin % -- Update --
        {ok, U0} = real_query(Config, <<"UPDATE " ?TABLE " SET extra = -1 WHERE id = 1">>),
        0 = affected_rows(U0), 0 = warning_count(U0),

        {ok, U1} = real_query(Config, <<"UPDATE " ?TABLE " SET extra = -1 WHERE id = 560">>),
        1 = affected_rows(U1), 0 = warning_count(U1),

        {ok, _, [ [-1.0] ], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end,

    begin % -- Delete --
        {ok, D0} = real_query(Config, <<"DELETE FROM " ?TABLE " WHERE id = 1">>),
        0 = affected_rows(D0), 0 = warning_count(D0),

        {ok, D1} = real_query(Config, <<"DELETE FROM " ?TABLE " WHERE id = 560">>),
        1 = affected_rows(D1), 0 = warning_count(D1),

        {ok, _, [], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end.

real_test_transaction(Config) ->
    real_test_transaction(Config, ?config(version,Config) > [5,1,0]).

real_test_transaction(_Config, false) ->
    {skip, not_supported};
real_test_transaction(Config, true) ->

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ") ENGINE = InnoDB">>),
    begin % -- commit --
        {ok, C1} = real_query(Config, <<"INSERT INTO " ?TABLE " VALUES"
                                        "  (400,'mysql',-4.0)"
                                        ", (410,'mysql',-4.1)"
                                        ", (500,'mysql',-5.0)"
                                        ", (510,'mysql',+5.1)"
                                      >>),
        4 = affected_rows(C1), 0 = insert_id(C1), 0 = warning_count(C1),

        {ok, _} = commit(Config),

        {ok, _, [ [2] ], _} =
            real_query(Config, <<"SELECT COUNT(*) FROM " ?TABLE " WHERE id >= 500">>)
    end,

    begin % -- rollback --
        {ok, C2} = real_query(Config, <<"INSERT INTO " ?TABLE " VALUES"
                                        "  (600,'mysql',+6.0)"
                                      >>),
        1 = affected_rows(C2), 0 = insert_id(C2), 0 = warning_count(C2),

        {ok, _, [ [3] ], _} =
            real_query(Config, <<"SELECT COUNT(*) FROM " ?TABLE " WHERE id >= 500">>),

        {ok, _} = rollback(Config),

        {ok, _, [ [2] ], _} =
            real_query(Config, <<"SELECT COUNT(*) FROM " ?TABLE " WHERE id >= 500">>)
    end,

    begin % -- commit --
        {ok, C3} = real_query(Config, <<"INSERT INTO " ?TABLE " VALUES"
                                        "  (550,'mysql',+5.5)"
                                        ", (560,'mysql',+5.6)"
                                      >>),
        2 = affected_rows(C3), 0 = insert_id(C3), 0 = warning_count(C3),

        {ok, _} = commit(Config),

        {ok, _, [ [4] ], _} =
            real_query(Config, <<"SELECT COUNT(*) FROM " ?TABLE " WHERE id >= 500">>)
    end.

real_test_multi(Config) ->
    real_test_multi(Config, ?config(version,Config) > [5,5,0]).

real_test_multi(_Config, false) ->
    {skip, not_supported};
real_test_multi(Config, true) ->

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),

    Query = <<"INSERT INTO " ?TABLE " VALUES (560,'mysql',+5.6);"
              "UPDATE " ?TABLE " SET extra = extra * -1 WHERE id = 560;"
              "SELECT extra FROM " ?TABLE " WHERE id = 560">>,

    {ok, R0} = real_query(Config, Query),
    1 = affected_rows(R0), 0 = insert_id(R0), 0 = warning_count(R0),

    true = more_results(R0),

    {ok, R1} = next_result(Config),
    1 = affected_rows(R1), 0 = insert_id(R1), 0 = warning_count(R1),

    true = more_results(R1),

    {ok, _, [ [-5.6] ], R2} = next_result(Config),
    undefined = affected_rows(R2), 0 = warning_count(R2),

    false = more_results(R2).

real_test_call_1(Config) ->
    real_test_call_1(Config, ?config(version,Config) > [5,5,0]).

real_test_call_1(_Config, false) ->
    {skip, not_supported};
real_test_call_1(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP PROCEDURE IF EXISTS " ?TABLE "_p1">>),

    {ok, _} = real_query(Config, <<"CREATE PROCEDURE " ?TABLE "_p1 ("
                                   "  IN i_in INT"
                                   ")"
                                   "BEGIN"
                                   "  SELECT i_in * 2 AS value;"
                                   "END">>),

    {ok, _, [ [2] ], R0} = real_query(Config, <<"CALL " ?TABLE "_p1(1)">>),
    undefined = affected_rows(R0), 0 = warning_count(R0),

    true = more_results(R0),

    {ok, R1} = next_result(Config),
    0 = affected_rows(R1), 0 = warning_count(R1),

    false = more_results(R1),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p1">>).

real_test_call_2(Config) ->
    real_test_call_2(Config, ?config(version,Config) > [5,5,0]).

real_test_call_2(_Config, false) ->
    {skip, not_supported};
real_test_call_2(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP PROCEDURE IF EXISTS " ?TABLE "_p2">>),

    {ok, _} = real_query(Config, <<"CREATE PROCEDURE " ?TABLE "_p2 ("
                                   "  IN i_in INT"
                                   ")"
                                   "BEGIN"
                                   "  SELECT i_in * 3 AS value;"
                                   "  SELECT i_in * 4 AS value;"
                                   "END">>),

    {ok, _, [ [3] ], R0} = real_query(Config, <<"CALL " ?TABLE "_p2(1)">>),
    undefined = affected_rows(R0), 0 = warning_count(R0),

    true = more_results(R0),

    {ok, _, [ [4] ], R1} = next_result(Config),
    undefined = affected_rows(R1), 0 = warning_count(R1),

    true = more_results(R1),

    {ok, R2} = next_result(Config),
    0 = affected_rows(R2), 0 = warning_count(R2),

    false = more_results(R2),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p2">>).

stmt_test_crud(Config) ->
    stmt_test_crud(Config, ?config(version,Config) > [5,1,0]).

stmt_test_crud(_Config, false) ->
    {skip, not_supported};
stmt_test_crud(Config, true) ->

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),
    begin % -- Create --
        {ok, C0} = stmt_prepare(Config, <<"INSERT INTO " ?TABLE " VALUES (?,?,?)">>),
        0 = stmt_warning_count(C0), 0 = stmt_field_count(C0), 3 = stmt_param_count(C0),

        {ok, C1} = stmt_execute(Config, C0, [560,<<"mysql">>,+5.6]),
        1 = stmt_affected_rows(C1), 0 = stmt_insert_id(C1), 0 = stmt_warning_count(C1),

        ok = stmt_close(Config, C1)
    end,

    begin % -- Read --
        {ok, R0} = stmt_prepare(Config, <<"SELECT * FROM " ?TABLE " WHERE id = ?">>),
        0 = stmt_warning_count(R0), 3 = stmt_field_count(R0), 1 = stmt_param_count(R0),

        {ok, _, [], R1} = stmt_execute(Config, R0, [1]),
        undefined = stmt_affected_rows(R1), 0 = stmt_warning_count(R1),

        {ok, R2} = stmt_reset(Config, R1),

        {ok, _, [ [560,<<"mysql">>,+5.6] ], R3} = stmt_execute(Config, R2, [560]),
        undefined = stmt_affected_rows(R3), 0 = stmt_warning_count(R3),

        ok = stmt_close(Config, R3)
    end,

    begin % -- Update --
        {ok, U0} = stmt_prepare(Config, <<"UPDATE " ?TABLE " SET extra = -1 WHERE id = ?">>),
        0 = stmt_warning_count(U0), 0 = stmt_field_count(U0), 1 = stmt_param_count(U0),

        {ok, U1} = stmt_execute(Config, U0, [1]),
        0 = stmt_affected_rows(U1), 0 = stmt_warning_count(U1),

        {ok, U2} = stmt_reset(Config, U1),

        {ok, U3} = stmt_execute(Config, U2, [560]),
        1 = stmt_affected_rows(U3), 0 = stmt_warning_count(U3),

        ok = stmt_close(Config, U3),

        {ok, _, [ [-1.0] ], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end,

    begin % -- Delete --
        {ok, D0} = stmt_prepare(Config, <<"DELETE FROM " ?TABLE " WHERE id = ?">>),
        0 = stmt_warning_count(D0), 0 = stmt_field_count(D0), 1 = stmt_param_count(D0),

        {ok, D1} = stmt_execute(Config, D0, [1]),
        0 = stmt_affected_rows(D1), 0 = stmt_warning_count(D1),

        {ok, D2} = stmt_reset(Config, D1),

        {ok, D3} = stmt_execute(Config, D2, [560]),
        1 = stmt_affected_rows(D3), 0 = stmt_warning_count(D3),

        ok = stmt_close(Config, D3),

        {ok, _, [], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end.

stmt_test_count_0_0(Config) ->
    stmt_test_count_0_0(Config, ?config(version,Config) > [5,1,0]).

stmt_test_count_0_0(_Config, false) ->
    {skip, not_supported};
stmt_test_count_0_0(Config, true) -> % param=0, field=0 (no-eof)

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),

    {ok, P0} = stmt_prepare(Config, <<"INSERT INTO " ?TABLE " VALUES (560,'mysql',+5.6)">>),
    undefined = stmt_affected_rows(P0), undefined = stmt_insert_id(P0),
    undefined = stmt_warning_count(P0), 0 = stmt_field_count(P0), 0 = stmt_param_count(P0),

    {ok, P1} = stmt_execute(Config, P0, []),
    1 = stmt_affected_rows(P1), 0 = stmt_insert_id(P1), 0 = stmt_warning_count(P1),

    ok = stmt_close(Config, P1).

stmt_test_count_0_1(Config) ->
    stmt_test_count_0_1(Config, ?config(version,Config) > [5,1,0]).

stmt_test_count_0_1(_Config, false) ->
    {skip, not_supported};
stmt_test_count_0_1(Config, true) -> % param=0, field=1

    {ok, P0} = stmt_prepare(Config, <<"SELECT @@version">>),
    0 = stmt_warning_count(P0), 1 = stmt_field_count(P0), 0 = stmt_param_count(P0),

    {ok, _, _, P1} = stmt_execute(Config, P0, []),
    undefined = stmt_affected_rows(P1), 0 = stmt_warning_count(P1),

    ok = stmt_close(Config, P1).

stmt_test_count_1_1(Config) ->
    stmt_test_count_1_1(Config, ?config(version,Config) > [5,1,0]).

stmt_test_count_1_1(_Config, false) ->
    {skip, not_supported};
stmt_test_count_1_1(Config, true) -> % param=1, field=1

    {ok, P0} = stmt_prepare(Config, <<"SHOW STATUS WHERE variable_name = ?">>),
    0 = stmt_warning_count(P0), 2 = stmt_field_count(P0), 1 = stmt_param_count(P0),

    {ok, _, _, P1} = stmt_execute(Config, P0, [<<"uptime">>]),
    undefined = stmt_affected_rows(P1), 0 = stmt_warning_count(P1),

    ok = stmt_close(Config, P1).

stmt_test_count_3_0(Config) ->
    stmt_test_count_3_0(Config, ?config(version,Config) > [5,1,0]).

stmt_test_count_3_0(_Config, false) ->
    {skip, not_supported};
stmt_test_count_3_0(Config, true) -> % param=3, field=0

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),

    {ok, P0} = stmt_prepare(Config, <<"INSERT INTO " ?TABLE " VALUES (?,?,?)">>),
    0 = stmt_warning_count(P0), 0 = stmt_field_count(P0), 3 = stmt_param_count(P0),

    {ok, P1} = stmt_execute(Config, P0, [560,<<"mysql">>,+5.6]),
    1 = stmt_affected_rows(P1), 0 = stmt_insert_id(P1), 0 = stmt_warning_count(P1),

    {ok, P2} = stmt_reset(Config, P1),

    {ok, P3} = stmt_execute(Config, P2, [550,<<"mysql">>,-5.5]),
    1 = stmt_affected_rows(P3), 0 = stmt_insert_id(P3), 0 = stmt_warning_count(P3),

    {ok, P4} = stmt_reset(Config, P3),

    {ok, P5} = stmt_execute(Config, P4, [510,<<"mysql">>,null]),
    1 = stmt_affected_rows(P5), 0 = stmt_insert_id(P5), 0 = stmt_warning_count(P5),

    {ok, P6} = stmt_reset(Config, P5),

    {ok, P7} = stmt_execute(Config, P6, [410,null,-4.1]),
    1 = stmt_affected_rows(P7), 0 = stmt_insert_id(P7), 0 = stmt_warning_count(P7),

    {ok, P8} = stmt_reset(Config, P7),

    {ok, P9} = stmt_execute(Config, P8, [null,<<"mysql">>,-4.0]),
    1 = stmt_affected_rows(P9), 0 = stmt_insert_id(P9), 0 = stmt_warning_count(P9),

    ok = stmt_close(Config, P9).

stmt_test_fetch(Config) ->
    stmt_test_fetch(Config, ?config(version,Config) > [5,1,0]).

stmt_test_fetch(_Config, false) ->
    {skip, not_supported};
stmt_test_fetch(Config, true) ->

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),

    {ok, _} = real_query(Config, <<"INSERT INTO " ?TABLE " VALUES"
                                   "  (400,'mysql',-4.0)"
                                   ", (410,'mysql',-4.1)"
                                   ", (500,'mysql',-5.0)"
                                   ", (510,'mysql',+5.1)"
                                   ", (550,'mysql',+5.5)"
                                   ", (560,'mysql',+5.6)"
                                 >>),

    {ok, P0} = stmt_prepare(Config, <<"SELECT extra FROM " ?TABLE
                                      " WHERE id > ? AND extra > ?">>),
    0 = stmt_warning_count(P0), 1 = stmt_field_count(P0), 2 = stmt_param_count(P0),

    ?CURSOR_TYPE_NO_CURSOR = stmt_attr_get(P0, ?STMT_ATTR_CURSOR_TYPE),
    1 = stmt_attr_get(P0, ?STMT_ATTR_PREFETCH_ROWS),

    P1 = stmt_attr_set(P0, ?STMT_ATTR_CURSOR_TYPE, ?CURSOR_TYPE_READ_ONLY),

    begin % -- prefetch=5 --

        P10 = stmt_attr_set(P1, ?STMT_ATTR_PREFETCH_ROWS, 4),

        {ok, P11} = stmt_execute(Config, P10, [-1,0]),
        undefined = stmt_affected_rows(P11), 0 = stmt_warning_count(P11),

        {ok, _, [ [+5.1], [+5.5], [+5.6] ], P12} = stmt_fetch(Config, P11),
        undefined = stmt_affected_rows(P12), 0 = stmt_warning_count(P12),

        {ok, P13} = stmt_fetch(Config, P12),
        undefined = stmt_affected_rows(P13), 0 = stmt_warning_count(P13),

        {ok, P14} = stmt_reset(Config, P13),
        0 = stmt_affected_rows(P14), 0 = stmt_warning_count(P14)
    end,

    begin % -- prefetch=4 --

        P20 = stmt_attr_set(P1, ?STMT_ATTR_PREFETCH_ROWS, 3),

        {ok, P21} = stmt_execute(Config, P20, [-1,0]),
        undefined = stmt_affected_rows(P21), 0 = stmt_warning_count(P21),

        {ok, _, [ [+5.1], [+5.5], [+5.6] ], P22} = stmt_fetch(Config, P21),
        undefined = stmt_affected_rows(P22), 0 = stmt_warning_count(P22),

        {ok, _, [], P23} = stmt_fetch(Config, P22), % ?!
        undefined = stmt_affected_rows(P23), 0 = stmt_warning_count(P23),

        {ok, P24} = stmt_fetch(Config, P23),
        undefined = stmt_affected_rows(P24), 0 = stmt_warning_count(P24),

        {ok, P25} = stmt_reset(Config, P24),
        0 = stmt_affected_rows(P25), 0 = stmt_warning_count(P25)
    end,

    begin % -- prefetch=3 --

        P30 = stmt_attr_set(P1, ?STMT_ATTR_PREFETCH_ROWS, 2),

        {ok, P31} = stmt_execute(Config, P30, [-1,0]),
        undefined = stmt_affected_rows(P31), 0 = stmt_warning_count(P31),

        {ok, _, [ [+5.1], [+5.5] ], P32} = stmt_fetch(Config, P31),
        undefined = stmt_affected_rows(P32), 0 = stmt_warning_count(P32),

        {ok, _, [ [+5.6] ], P33} = stmt_fetch(Config, P32),
        undefined = stmt_affected_rows(P33), 0 = stmt_warning_count(P33),

        {ok, P34} = stmt_fetch(Config, P33),
        undefined = stmt_affected_rows(P34), 0 = stmt_warning_count(P34),

        {ok, P35} = stmt_reset(Config, P34),
        0 = stmt_affected_rows(P35), 0 = stmt_warning_count(P35)
    end,

    ok = stmt_close(Config, P1).

stmt_test_call_1(Config) ->
    stmt_test_call_1(Config, ?config(version,Config) > [5,5,0]).

stmt_test_call_1(_Config, false) ->
    {skip, not_supported};
stmt_test_call_1(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP PROCEDURE IF EXISTS " ?TABLE "_p1">>),

    {ok, _} = real_query(Config, <<"CREATE PROCEDURE " ?TABLE "_p1 ("
                                   "  IN i_in INT"
                                   ")"
                                   "BEGIN"
                                   "  SELECT i_in * 2 AS value;"
                                   "END">>),

    {ok, P0} = stmt_prepare(Config, <<"CALL " ?TABLE "_p1(?)">>),
    0 = stmt_warning_count(P0), 0 = stmt_field_count(P0), 1 = stmt_param_count(P0),

    {ok, _, [ [2] ], P1} = stmt_execute(Config, P0, [1]),
    undefined = stmt_affected_rows(P1), 0 = stmt_warning_count(P1),

    true = more_results(P1),

    {ok, P2} = stmt_next_result(Config, P1),
    0 = stmt_affected_rows(P2), 0 = stmt_warning_count(P2),

    false = more_results(P2),

    ok = stmt_close(Config, P2),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p1">>).

stmt_test_call_2(Config) ->
    stmt_test_call_2(Config, ?config(version,Config) > [5,5,0]).

stmt_test_call_2(_Config, false) ->
    {skip, not_supported};
stmt_test_call_2(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP PROCEDURE IF EXISTS " ?TABLE "_p2">>),

    {ok, _} = real_query(Config, <<"CREATE PROCEDURE " ?TABLE "_p2 ("
                                   "  IN i_in INT"
                                   ")"
                                   "BEGIN"
                                   "  SELECT i_in * 3 AS value;"
                                   "  SELECT i_in * 4 AS value;"
                                   "END">>),

    {ok, P0} = stmt_prepare(Config, <<"CALL " ?TABLE "_p2(?)">>),
    0 = stmt_warning_count(P0), 0 = stmt_field_count(P0), 1 = stmt_param_count(P0),

    {ok, _, [ [3] ], P1} = stmt_execute(Config, P0, [1]),
    undefined = stmt_affected_rows(P1), 0 = stmt_warning_count(P1),

    true = more_results(P1),

    {ok, _, [ [4] ], P2} = stmt_execute(Config, P1, [1]),
    undefined = stmt_affected_rows(P2), 0 = stmt_warning_count(P2),

    true = more_results(P2),

    {ok, P3} = stmt_next_result(Config, P2),
    0 = stmt_affected_rows(P3), 0 = stmt_warning_count(P3),

    false = more_results(P3),

    ok = stmt_close(Config, P3),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p2">>).

%% == group: blob_test ==

stmt_test_blob(Config) ->

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", value LONGBLOB"
                                   ")">>),

    D = case ?config(compress,Config) of
            true ->
                [
                 225, 226, 251,
                 65508, 65509, 65536,
                 16777183, 16777184, 16777215, 16777216
                ];
            false ->
                [
                 221, 222, 251,
                 65504, 65505, 65536,
                 16777184, 16777185, 16777215, 16777216
                ]
        end,

    L = begin % -- Create --

            {ok, C0} = stmt_prepare(Config, <<"INSERT INTO " ?TABLE " VALUES (?,?)">>),
            0 = stmt_warning_count(C0), 0 = stmt_field_count(C0), 2 = stmt_param_count(C0),

            F = fun(E, A) ->

                        B = list_to_binary([ crypto:rand_bytes(1) || _ <- lists:seq(1,E)]),

                        {ok, P} = stmt_execute(Config,A,[E,B]),
                        1 = stmt_affected_rows(P), 0 = stmt_warning_count(P),

                        { [E,myer_debug:md5(B)], P}
                end,
            {L1, C1} = lists:mapfoldl(F, C0, D),

            ok = stmt_close(Config, C1),

            L1
        end,

    begin % -- Read --

        Q = <<"SELECT id, MD5(value) AS x FROM " ?TABLE " ORDER BY id">>,

        {ok, _, L, R} = real_query(Config, Q),
        undefined = affected_rows(R), 0 = warning_count(R)
    end.

%% == group: other_test ==

ping_test(Config) ->
    {ok, _} = ping(Config).

stat_test(Config) ->
    {ok, _} = stat(Config).

refresh_test(Config) ->
    refresh_test(Config, ?config(version,Config) > [5,1,0]).

refresh_test(_Config, false) ->
    {skip, not_supported};
refresh_test(Config, true) ->
    {ok, _} = refresh(Config, ?REFRESH_GRANT),
    {ok, _} = refresh(Config, ?REFRESH_LOG),
    {ok, _} = refresh(Config, ?REFRESH_TABLES),
    {ok, _} = refresh(Config, ?REFRESH_HOSTS),
    {ok, _} = refresh(Config, ?REFRESH_STATUS),
    {ok, _} = refresh(Config, ?REFRESH_THREADS),
    {ok, _} = refresh(Config, ?REFRESH_SLAVE),
    {ok, _} = refresh(Config, ?REFRESH_MASTER),
    {ok, _} = refresh(Config, ?REFRESH_ERROR_LOG),
    {ok, _} = refresh(Config, ?REFRESH_ENGINE_LOG),
    {ok, _} = refresh(Config, ?REFRESH_BINARY_LOG),
    {ok, _} = refresh(Config, ?REFRESH_RELAY_LOG),
    {ok, _} = refresh(Config, ?REFRESH_GENERAL_LOG),
    {ok, _} = refresh(Config, ?REFRESH_SLOW_LOG).

select_db_test(Config) ->
    select_db_test(Config, ?config(version,Config) > [5,1,0]).

select_db_test(_Config, false) ->
    {skip, not_supported};
select_db_test(Config, true) ->

    {error, R} = select_db(Config, <<"nowhere">>),
    1044 = errno(R), <<"42000">> = sqlstate(R),
    <<"Access denied for user ",_/binary>> = errmsg(R),

    {ok, _} = select_db(Config, <<"test">>).

%% == group: cover_test ==

cover_myer(_Config) ->

    {ok, [0,1,0]} = myer:get_client_version().

cover_myer_app(Config) ->

    {ok, _} = myer_app:call(?config(pool,Config), {real_query,[<<"SELECT @@version">>]}),

    {error, badarg} = myer_app:call(nobody, {real_query,[<<"SELECT @@version">>]}),

    {error, badarg} = myer_app:checkout(nobody, false, 1),

    {error, badarg} = myer_app:checkin(nobody, self()),

    F = fun (_,A) ->
                case myer_app:checkout(?config(pool,Config),false,5) of
                    {ok, Pid} ->
                        [Pid|A];
                    {error, full} ->
                        throw(A)
                end
        end,
    case catch lists:foldl(F, [], lists:seq(1,1000)) of
        {'EXIT',_} -> % gen_server:call, timeout ...
            ok;
        List ->
            ct:log("length=~p", [length(List)]),
            lists:foreach(fun(E) -> myer_app:checkin(?config(pool,Config),E) end, List)
    end.

cover_myer_client(Config) ->

    Pid = ?config(pid,Config),

    {error,badarg} = myer_client:call(Pid, ?MODULE),

    ok = myer_client:cast(Pid, ?MODULE),

    Pid ! ?MODULE,

    ok.

%% -- --

call(Func, Args) ->
    apply(myer, Func, Args).

call(Config, Func, Args) ->
    apply(myer, Func, [?config(pid,Config)|Args]).

cleanup(Config) ->
    lists:foldl(fun proplists:delete/2, Config, [version]).

close(Config) ->
    case myer:close(?config(pool,Config), ?config(pid,Config)) of
        ok ->
            proplists:delete(pid, Config);
        {error, Reason} ->
            ct:fail(Reason)
    end.

connect(Config) ->
    case myer:connect(?config(pool,Config)) of
        {ok, Pid} ->
            ct:log("connect, pid=~p", [Pid]),
            [{pid,Pid}|Config];
        {error, Reason} ->
            ct:fail(Reason)
    end.

drop_table(Config) ->
    _ = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE >>),
    Config.

get_server_version(Config) ->
    case call(Config, get_server_version, []) of
        {ok, Version} ->
            [{version,Version}|Config];
        {error, Reason} ->
            ct:fail(Reason)
    end.

set_env(Config) ->
    A = ?config(pool, Config),
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {compress, ?config(compress,Config)}
        ],
    ok = application:set_env(myer, poolboy,
                             [{A,[{size,1},{max_overflow,3}],L ++ ct:get_config(A)}]),
    Config.

start(Config) ->
    case myer:start() of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

stop(Config) ->
    case myer:stop() of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

testcase_post(Config) ->
    {ok, _} = commit(Config),
    {ok, _} = autocommit(Config, true),
    Config.

testcase_pre(Config) ->
    {ok, _} = autocommit(Config, false),
    Config.

autocommit(Config, Bool) -> call(Config, autocommit, [Bool]).
commit(Config) -> call(Config, commit, []).
next_result(Config) -> call(Config, next_result, []).
ping(Config) -> call(Config, ping, []).
real_query(Config, Query) -> call(Config, real_query, [Query]).
refresh(Config, Options) -> call(Config, refresh, [Options]).
rollback(Config) -> call(Config, rollback, []).
select_db(Config, Database) -> call(Config, select_db, [Database]).
stat(Config) -> call(Config, stat, []).
stmt_close(Config, Prepare) -> call(Config, stmt_close, [Prepare]).
stmt_execute(Config, Prepare, Args) -> call(Config, stmt_execute, [Prepare,Args]).
stmt_fetch(Config, Prepare) -> call(Config, stmt_fetch, [Prepare]).
stmt_prepare(Config, Query) -> call(Config, stmt_prepare, [Query]).
stmt_reset(Config, Prepare) -> call(Config, stmt_reset, [Prepare]).
stmt_next_result(Config, Prepare) -> call(Config, stmt_next_result, [Prepare]).

affected_rows(Result) -> call(affected_rows, [Result]).
errno(Reason) -> call(errno, [Reason]).
errmsg(Reason) -> call(errmsg, [Reason]).
insert_id(Record) -> call(insert_id, [Record]).
more_results(Record) -> call(more_results, [Record]).
sqlstate(Reason) -> call(sqlstate, [Reason]).
stmt_affected_rows(Prepare) -> call(stmt_affected_rows, [Prepare]).
stmt_field_count(Prepare) -> call(stmt_field_count, [Prepare]).
stmt_insert_id(Prepare) -> call(stmt_insert_id, [Prepare]).
stmt_param_count(Prepare) -> call(stmt_param_count, [Prepare]).
stmt_attr_get(Prepare, AttrType) -> call(stmt_attr_get, [Prepare,AttrType]).
stmt_attr_set(Prepare, AttrType, Value) -> call(stmt_attr_set, [Prepare,AttrType,Value]).
stmt_warning_count(Prepare) -> call(stmt_warning_count, [Prepare]).
warning_count(Result) -> call(warning_count, [Result]).
