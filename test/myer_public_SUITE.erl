%% =============================================================================
%% =============================================================================

-module(myer_public_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0,
         groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% -- public --
-export([start_test/1, stop_test/1, version_test/1]).
-export([checkout_test/1, checkin_test/1]).
-export([set_timeout_test/1]).

-export([get_server_version_test/1, stat_test/1]).
-export([ping_test/1, refresh_test/1, select_db_test/1]).

-export([real_test_crud/1,
         real_test_transaction/1,
         real_test_multi/1,
         real_test_call_1/1, real_test_call_2/1]).
-export([stmt_test_crud/1,
         stmt_test_multi/1,
         stmt_test_call_1/1, stmt_test_call_2/1
        %stmt_test_blob/1
        ]).

-export([cover_myer/1]).

%% -- internal --
-define(TABLE, ?MODULE_STRING). % TODO (sequence -> parallel)

%% == callback: ct ==

all() -> [
          version_test,
          {group, groups_public}
         ].

groups() -> [

             {groups_public, [sequence], [
                                          {group, group_normal},
                                          {group, group_compress}
                                         ]},

             {group_normal, [sequence], [
                                         set_timeout_test,
                                         {group, group_query},
                                         {group, group_other},
                                         {group, group_cover}
                                        ]},

             {group_compress, [sequence], [
                                         % {group, group_query}
                                          ]},

             {group_query, [], [
                                real_test_crud,
                                real_test_transaction,
                                real_test_multi,
                                real_test_call_1, real_test_call_2,
                                stmt_test_crud,
                                stmt_test_multi,
                                stmt_test_call_1, stmt_test_call_2
                              % stmt_test_blob
                               ]},

             {group_other, [], [
                                ping_test,
                                refresh_test,
                                select_db_test,
                                stat_test
                               ]},

             {group_cover, [], [
                                cover_myer
                               ]}
            ].

init_per_group(Group, Config) ->
    case ct:get_config(Group) of
        undefined ->
            Config;
        List ->
            ok = set_env(List),
            L = [fun start_test/1],
            lists:foldl(fun(E,A) -> E(A) end, Config, L)
    end.

end_per_group(Group, Config) ->
    case ct:get_config(Group) of
        undefined ->
            Config;
        _ ->
            L = [fun stop_test/1],
            lists:foldl(fun(E,A) -> E(A) end, Config, L)
    end.

init_per_testcase(version_test, Config) ->
    Config;
init_per_testcase(TestCase, Config) ->
    case atom_to_binary(TestCase, latin1) of
        <<"group", _/binary>> ->
            Config;
        _ ->
            L = [ fun checkout_test/1, fun get_server_version_test/1, fun setup/1 ],
            lists:foldl(fun(E,A) -> E(A) end, Config, L)
    end.

end_per_testcase(version_test, Config) ->
    Config;
end_per_testcase(TestCase, Config) ->
    case atom_to_binary(TestCase, latin1) of
        <<"group", _/binary>> ->
            Config;
        _ ->
            L = [fun cleanup/1, fun checkin_test/1 ],
            lists:foldl(fun(E,A) -> E(A) end, Config, L)
    end.

%% == public ==

start_test(Config) ->
    case call(start, []) of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

stop_test(Config) ->
    case call(stop, []) of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

version_test(Config) ->
    [0,3,0] = version(Config).


checkout_test(Config) ->
    case call(checkout, [mysql_pool]) of
        {ok, Handle} ->
            [{handle,Handle}|Config];
        {error, Reason} ->
            ct:fail(Reason)
    end.

checkin_test(Config) ->
    case call(Config, checkin, []) of
        ok ->
            proplists:delete(handle, Config);
        {error, Reason} ->
            ct:fail(Reason)
    end.


set_timeout_test(Config) ->
    {ok, _} = call(Config, set_timeout, [timer:minutes(10)]).


get_server_version_test(Config) ->
    case call(Config, get_server_version, []) of
        {ok, Version} ->
            [{version,Version}|Config];
        {error, Reason} ->
            ct:fail(Reason)
    end.

ping_test(Config) ->
    {ok, _} = ping(Config).

refresh_test(Config) ->
    refresh_test(Config, ?config(version,Config) > [5,1,0] andalso not(travis())).

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

stat_test(Config) ->
    {ok, _} = stat(Config).

%% -- real_* --

real_test_crud(Config) ->
    real_test_crud(Config, ?config(version,Config) > [5,1,0]).

real_test_crud(_Config, false) ->
    {skip, not_supported};
real_test_crud(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE>>),

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
        undefined = affected_rows(R), undefined = insert_id(R), 0 = warning_count(R)
    end,

    begin % -- Update --
        {ok, U0} = real_query(Config, <<"UPDATE " ?TABLE " SET extra = -1 WHERE id = 1">>),
        0 = affected_rows(U0), 0 = insert_id(U0), 0 = warning_count(U0),

        {ok, U1} = real_query(Config, <<"UPDATE " ?TABLE " SET extra = -1 WHERE id = 560">>),
        1 = affected_rows(U1), 0 = insert_id(U1), 0 = warning_count(U1),

        {ok, _, [ [-1.0] ], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end,

    begin % -- Delete --
        {ok, D0} = real_query(Config, <<"DELETE FROM " ?TABLE " WHERE id = 1">>),
        0 = affected_rows(D0), 0 = insert_id(D0), 0 = warning_count(D0),

        {ok, D1} = real_query(Config, <<"DELETE FROM " ?TABLE " WHERE id = 560">>),
        1 = affected_rows(D1), 0 = insert_id(D1), 0 = warning_count(D1),

        {ok, _, [], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end.

real_test_transaction(Config) ->
    real_test_transaction(Config, ?config(version,Config) > [5,1,0]).

real_test_transaction(_Config, false) ->
    {skip, not_supported};
real_test_transaction(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE>>),

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

    {ok, _} = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE>>),

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
    undefined = affected_rows(R2), undefined = insert_id(R2), 0 = warning_count(R2),

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

    {ok, _, [ [2] ], P0} = real_query(Config, <<"CALL " ?TABLE "_p1(1)">>),
    undefined = affected_rows(P0), 0 = warning_count(P0),

    true = more_results(P0),

    {ok, P1} = next_result(Config),
    0 = affected_rows(P1), 0 = insert_id(P1), 0 = warning_count(P1),

    false = more_results(P1),

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

    {ok, _, [ [3] ], P0} = real_query(Config, <<"CALL " ?TABLE "_p2(1)">>),
    undefined = affected_rows(P0), undefined = insert_id(P0), 0 = warning_count(P0),

    true = more_results(P0),

    {ok, _, [ [4] ], P1} = next_result(Config),
    undefined = affected_rows(P1), undefined = insert_id(P1), 0 = warning_count(P1),

    true = more_results(P1),

    {ok, P2} = next_result(Config),
    0 = affected_rows(P2), 0 = insert_id(P2), 0 = warning_count(P2),

    false = more_results(P2),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p2">>).

%% -- stmt_* --

stmt_test_crud(Config) ->
    stmt_test_crud(Config, ?config(version,Config) > [5,1,0]).

stmt_test_crud(_Config, false) ->
    {skip, not_supported};
stmt_test_crud(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE>>),

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),

    Name = list_to_binary(?TABLE),

    begin % -- Create --
        {ok, C0} = prepare(Config, Name, <<"INSERT INTO " ?TABLE " VALUES (?,?,?)">>),
        0 = affected_rows(C0), 0 = warning_count(C0),

        {ok, C1} = execute(Config, Name, [560,<<"mysql">>,+5.6]),
        1 = affected_rows(C1), 0 = insert_id(C1), 0 = warning_count(C1),

        {ok, _} = unprepare(Config, Name)
    end,

    begin % -- Read --
        {ok, R0} = prepare(Config, Name, <<"SELECT * FROM " ?TABLE " WHERE id = ?">>),
        0 = affected_rows(R0), 0 = warning_count(R0),

        {ok, _, [ [560,<<"mysql">>,+5.6] ], R1} = execute(Config, Name, [560]),
        undefined = affected_rows(R1), undefined = insert_id(R1), 0 = warning_count(R1),

        {ok, _} = unprepare(Config, Name)
    end,

    begin % -- Update --
        {ok, U0} = prepare(Config, Name, <<"UPDATE " ?TABLE " SET extra = -1 WHERE id = ?">>),
        0 = affected_rows(U0), 0 = warning_count(U0),

        {ok, U1} = execute(Config, Name, [1]),
        0 = affected_rows(U1), 0 = insert_id(U1), 0 = warning_count(U1),

        {ok, U2} = execute(Config, Name, [560]),
        1 = affected_rows(U2), 0 = insert_id(U2), 0 = warning_count(U2),

        {ok, _} = unprepare(Config, Name),

        {ok, _, [ [-1.0] ], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end,

    begin % -- Delete --
        {ok, D0} = prepare(Config, Name, <<"DELETE FROM " ?TABLE " WHERE id = ?">>),
        0 = affected_rows(D0), 0 = warning_count(D0),

        {ok, D1} = execute(Config, Name, [1]),
        0 = affected_rows(D1), 0 = insert_id(D1), 0 = warning_count(D1),

        {ok, D2} = execute(Config, Name, [560]),
        1 = affected_rows(D2), 0 = insert_id(D2), 0 = warning_count(D2),

        {ok, _} = unprepare(Config, Name),

        {ok, _, [], _} =
            real_query(Config, <<"SELECT extra FROM " ?TABLE " WHERE id = 560">>)
    end.


stmt_test_multi(Config) ->
    stmt_test_multi(Config, ?config(version,Config) > [5,5,0]).

stmt_test_multi(_Config, false) ->
    {skip, not_supported};
stmt_test_multi(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE>>),

    {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
                                   "  id    INT"
                                   ", name  VARCHAR(5)"
                                   ", extra DOUBLE"
                                   ")">>),

    Name = list_to_binary(?TABLE),

    Query = <<"INSERT INTO " ?TABLE " VALUES (?,'mysql',+5.6);"
              "UPDATE " ?TABLE " SET extra = extra * -1 WHERE id = ?;"
              "SELECT extra FROM " ?TABLE " WHERE id = ?">>,

    %% {ok, R0} = prepare(Config, Name, Query),
    %% 0 = affected_rows(R0), 0 = warning_count(R0),

    %% {ok, R1} = execute(Config, Name, [560,560,560]),
    %% 1 = affected_rows(R1), 0 = insert_id(R1), 0 = warning_count(R1),

    %% true = more_results(R1),

    %% {ok, R2} = next_result(Config),
    %% 1 = affected_rows(R2), 0 = insert_id(R2), 0 = warning_count(R2),

    %% true = more_results(R2),

    %% {ok, _, [ [-5.6] ], R3} = next_result(Config),
    %% undefined = affected_rows(R3), undefined = insert_id(R3), 0 = warning_count(R3),

    %% false = more_results(R3),

    %% {ok, _} = unprepare(Config, Name).

    {error, _} = prepare(Config, Name, Query). % TODO


stmt_test_call_1(Config) ->
    stmt_test_call_1(Config, ?config(version,Config) > [5,5,0]).

stmt_test_call_1(_Config, false) ->
    {skip, not_supported};
stmt_test_call_1(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP PROCEDURE IF EXISTS " ?TABLE "_p1">>),

    {ok, _} = real_query(Config, <<"CREATE PROCEDURE " ?TABLE "_p1 ("
                                   "  INOUT i_inout INT"
                                   ")"
                                   "BEGIN"
                                   "  SELECT i_inout * 2 AS i_inout;"
                                   "END">>),

    Name = list_to_binary(?TABLE),

    {ok, P0} = prepare(Config, Name, <<"CALL " ?TABLE "_p1(?)">>),
    0 = affected_rows(P0), 0 = warning_count(P0),

    {ok, _, [ [2] ], P1} = execute(Config, Name, [1]),
    undefined = affected_rows(P1), undefined = insert_id(P1), 0 = warning_count(P1),

    true = more_results(P1),

    {ok, P2} = next_result(Config),
    0 = affected_rows(P2), 0 = insert_id(P2), 0 = warning_count(P2),

    false = more_results(P2),

    {ok, _} = unprepare(Config, Name),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p1">>).

stmt_test_call_2(Config) ->
    stmt_test_call_2(Config, ?config(version,Config) > [5,5,0]).

stmt_test_call_2(_Config, false) ->
    {skip, not_supported};
stmt_test_call_2(Config, true) ->

    {ok, _} = real_query(Config, <<"DROP PROCEDURE IF EXISTS " ?TABLE "_p2">>),

    {ok, _} = real_query(Config, <<"CREATE PROCEDURE " ?TABLE "_p2 ("
                                   "  INOUT i_inout INT"
                                   ")"
                                   "BEGIN"
                                   "  SELECT i_inout * 3 AS i_inout;"
                                   "  SELECT i_inout * 4 AS i_inout;"
                                   "END">>),

    Name = list_to_binary(?TABLE),

    {ok, P0} = prepare(Config, Name, <<"CALL " ?TABLE "_p2(?)">>),
    0 = affected_rows(P0), 0 = warning_count(P0),

    {ok, _, [ [3] ], P1} = execute(Config, Name, [1]),
    undefined = affected_rows(P1), undefined = insert_id(P1), 0 = warning_count(P1),

    true = more_results(P1),

    {ok, _, [ [4] ], P2} = next_result(Config),
    undefined = affected_rows(P2), undefined = insert_id(P2), 0 = warning_count(P2),

    true = more_results(P2),

    {ok, P3} = next_result(Config),
    0 = affected_rows(P3), 0 = insert_id(P3), 0 = warning_count(P3),

    false = more_results(P3),

    {ok, _} = unprepare(Config, Name),

    {ok, _} = real_query(Config, <<"DROP PROCEDURE " ?TABLE "_p2">>).


%% stmt_test_blob(Config) ->

%%     {ok, _} = real_query(Config, <<"DROP TABLE IF EXISTS " ?TABLE>>),

%%     {ok, _} = real_query(Config, <<"CREATE TABLE " ?TABLE " ("
%%                                    "  id    INT"
%%                                    ", value LONGBLOB"
%%                                    ")">>),

%%     {ok, L} = application:get_env(myer, mysql_pool),

%%     D = case proplists:get_value(compress, L) of
%%             true ->
%%                 [
%%                  225, 226, 251,
%%                  65508, 65509, 65536,
%%                  16777183, 16777184, 16777215, 16777216
%%                 ];
%%             false ->
%%                 [
%%                  221, 222, 251,
%%                  65504, 65505, 65536,
%%                  16777184, 16777185, 16777215, 16777216
%%                 ]
%%         end,

%%     L = begin % -- Create --

%%             {ok, C0} = stmt_prepare(Config, <<"INSERT INTO " ?TABLE " VALUES (?,?)">>),
%%             0 = stmt_warning_count(C0), 0 = stmt_field_count(C0), 2 = stmt_param_count(C0),

%%             F = fun(E, A) ->

%%                         B = list_to_binary([ crypto:rand_bytes(1) || _ <- lists:seq(1,E)]),

%%                         {ok, P} = stmt_execute(Config,A,[E,B]),
%%                         1 = stmt_affected_rows(P), 0 = stmt_warning_count(P),

%%                         { [E,crypto:hash(md5,B)], P} % TODO
%%                 end,
%%             {L1, C1} = lists:mapfoldl(F, C0, D),

%%             ok = stmt_close(Config, C1),

%%             L1
%%         end,

%%     begin % -- Read --

%%         Q = <<"SELECT id, MD5(value) AS x FROM " ?TABLE " ORDER BY id">>,

%%         {ok, _, L, R} = real_query(Config, Q),
%%         undefined = affected_rows(R), 0 = warning_count(R)
%%     end.


cover_myer(_Config) ->

    {error, notfound} = call(checkout, [?MODULE]),

    undefined = call(affected_rows,      [?MODULE]),
    undefined = call(errno,              [?MODULE]),
    undefined = call(errmsg,             [?MODULE]),
    undefined = call(insert_id,          [?MODULE]),
    false     = call(more_results,       [?MODULE]),
    undefined = call(sqlstate,           [?MODULE]),
    undefined = call(warning_count,      [?MODULE]),

    ok.

%% == internal ==

cleanup(Config) ->
    {ok, _} = autocommit(Config, true),
    lists:foldl(fun proplists:delete/2, Config, [version]).

setup(Config) ->
    {ok, _} = autocommit(Config, false),
    Config.


call(Function, Args) -> test(myer, Function, Args).
call(Config, Function, Args) -> call(Config, myer, Function, Args).
call(Config, Module, Function, Args) -> test(Module, Function, [?config(handle,Config)|Args]).
set_env(List) -> baseline_ct:set_env(List).
test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).

version(_Config) -> call(version, []).

autocommit(Config, Bool) -> call(Config, autocommit, [Bool]).
commit(Config) -> call(Config, commit, []).
execute(Config, Name, Params) -> call(Config, execute, [Name,Params]).
next_result(Config) -> call(Config, next_result, []).
ping(Config) -> call(Config, ping, []).
prepare(Config, Name, Query) -> call(Config, prepare, [Name,Query]).
real_query(Config, Query) -> call(Config, real_query, [Query]).
refresh(Config, Options) -> call(Config, refresh, [Options]).
rollback(Config) -> call(Config, rollback, []).
select_db(Config, Database) -> call(Config, select_db, [Database]).
stat(Config) -> call(Config, stat, []).
unprepare(Config, Name) -> call(Config, unprepare, [Name]).

affected_rows(Result) -> call(affected_rows, [Result]).
errno(Reason) -> call(errno, [Reason]).
errmsg(Reason) -> call(errmsg, [Reason]).
insert_id(Record) -> call(insert_id, [Record]).
more_results(Record) -> call(more_results, [Record]).
sqlstate(Reason) -> call(sqlstate, [Reason]).
warning_count(Result) -> call(warning_count, [Result]).

travis() -> false =/= os:getenv("TRAVIS_OTP_RELEASE").
