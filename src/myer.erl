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

-module(myer).

-include("myer_internal.hrl").

%% -- public: application --
-export([start/0, stop/0, get_client_version/0]).

%% -- public: pool --
-export([connect/1, connect/2, connect/3, close/2]).

%% -- public: worker --
-export([autocommit/2, commit/1, get_server_version/1, next_result/1, ping/1,
         real_query/2, refresh/2, rollback/1, select_db/2, stat/1]).
-export([stmt_close/2, stmt_execute/3, stmt_fetch/2, stmt_prepare/2, stmt_reset/2,
         stmt_next_result/2]).

%% -- public: record --
-export([affected_rows/1, errno/1, errmsg/1, insert_id/1, more_results/1, sqlstate/1,
         warning_count/1]).
-export([stmt_affected_rows/1, stmt_field_count/1, stmt_insert_id/1, stmt_param_count/1,
         stmt_attr_get/2, stmt_attr_set/3, stmt_warning_count/1]).

%% == public: application ==

-spec start() -> ok|{error,_}.
start() ->
    ok = lists:foreach(fun application:start/1, myer_app:deps()),
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec get_client_version() -> {ok,[non_neg_integer()]}.
get_client_version() ->
    {ok, myer_app:version()}.

%% == public: pool ==

-spec connect(atom()) -> {ok,pid()}|{error,_}.
connect(Pool)
  when is_atom(Pool) ->
    connect(Pool, false).

-spec connect(atom(),boolean()) -> {ok,pid()}|{error,_}.
connect(Pool, Block)
  when is_atom(Pool), is_boolean(Block) ->
    connect(Pool, Block, timer:seconds(5)).

-spec connect(atom(),boolean(),timeout()) -> {ok,pid()}|{error,_}.
connect(Pool, Block, Timeout)
  when is_atom(Pool), is_boolean(Block) ->
    myer_app:checkout(Pool, Block, Timeout).

-spec close(atom(),pid()) -> ok|{error,_}.
close(Pool, Worker)
  when is_atom(Pool), is_pid(Worker) ->
    myer_app:checkin(Pool, Worker).

%% == public: worker ==

-spec autocommit(pid(),boolean()) -> {ok,result()}|{error,_}.
autocommit(Pid, true)
  when is_pid(Pid) ->
    real_query(Pid, <<"SET autocommit=1">>);
autocommit(Pid, false)
  when is_pid(Pid) ->
    real_query(Pid, <<"SET autocommit=0">>).

-spec commit(pid()) -> {ok,result()}|{error,_}.
commit(Pid)
  when is_pid(Pid) ->
    real_query(Pid, <<"COMMIT">>).

-spec get_server_version(pid()) -> {ok,[non_neg_integer()]}|{error,_}.
get_server_version(Pid)
  when is_pid(Pid) ->
    call(Pid, {version,[]}).

-spec next_result(pid()) -> {ok,result()}|{ok,[field()],[term()],result()}|{error,_}.
next_result(Pid)
  when is_pid(Pid) ->
    call(Pid, {next_result,[]}).

-spec ping(pid()) -> {ok,result()}|{error,_}.
ping(Pid)
  when is_pid(Pid) ->
    call(Pid, {ping,[]}).

-spec real_query(pid(),binary()) -> {ok,result()}|{ok,[field()],[term()],result()}|{error,_}.
real_query(Pid, Query)
  when is_pid(Pid), is_binary(Query) ->
    call(Pid, {real_query,[Query]}).

-spec refresh(pid(),integer()) -> {ok,result()}|{error,_}.
refresh(Pid, Option)
  when is_pid(Pid), is_integer(Option) ->
    call(Pid, {refresh,[Option]}).

-spec rollback(pid()) -> {ok,result()}|{error,_}.
rollback(Pid)
  when is_pid(Pid) ->
    real_query(Pid, <<"ROLLBACK">>).

-spec select_db(pid(),binary()) -> {ok,result()}|{error,_}.
select_db(Pid, Database)
  when is_pid(Pid), is_binary(Database) ->
    call(Pid, {select_db,[Database]}).

-spec stat(pid()) -> {ok,binary()}|{error,_}.
stat(Pid)
  when is_pid(Pid) ->
    call(Pid, {stat,[]}).

-spec stmt_close(pid(),prepare()) -> ok|{error,_}.
stmt_close(Pid, #prepare{}=X)
  when is_pid(Pid) ->
    call(Pid, {stmt_close,[X]}).

-spec stmt_execute(pid(),prepare(),[term()])
                  -> {ok,prepare()}|{ok,[field()],[term()],prepare()}|{error,_}.
stmt_execute(Pid, #prepare{param_count=N}=X, Args)
  when is_pid(Pid), is_list(Args), N == length(Args) ->
    call(Pid, {stmt_execute,[X,Args]}).

-spec stmt_fetch(pid(),prepare())
                -> {ok,prepare()}|
                   {ok,[field()],[term()],prepare()}|{error,_}.
stmt_fetch(Pid, #prepare{}=X)
  when is_pid(Pid) ->
    call(Pid, {stmt_fetch,[X]}).

-spec stmt_prepare(pid(),binary()) -> {ok,prepare()}|{error,_}.
stmt_prepare(Pid, Query)
  when is_pid(Pid), is_binary(Query) ->
    call(Pid, {stmt_prepare,[Query]}).

-spec stmt_reset(pid(),prepare()) -> {ok,prepare()}|{error,_}.
stmt_reset(Pid, #prepare{}=X)
  when is_pid(Pid) ->
    call(Pid, {stmt_reset,[X]}).

-spec stmt_next_result(pid(),prepare())
                      -> {ok,prepare()}|{ok,[field()],[term()],prepare()}|{error,_}.
stmt_next_result(Pid, #prepare{}=X)
  when is_pid(Pid) ->
    call(Pid, {stmt_next_result,[X]}).

%% stmt_send_long_data, TODO

%% == public: record ==

-spec affected_rows(result()) -> non_neg_integer()|undefined.
affected_rows(#result{affected_rows=A}) -> A;
affected_rows(_) -> undefined.

-spec errno(reason()) -> non_neg_integer().
errno(#reason{errno=E}) -> E.

-spec errmsg(reason()) -> binary().
errmsg(#reason{message=M}) -> M. % rename error/1 to errmsg/1

-spec insert_id(result()) -> non_neg_integer()|undefined.
insert_id(#result{insert_id=I}) -> I;
insert_id(_) -> undefined.

-spec more_results(prepare()|result()) -> boolean().
more_results(#prepare{result=R}) ->
    more_results(R);
more_results(#result{status=S})
  when ?ISSET(S,?SERVER_MORE_RESULTS_EXISTS) ->
    true;
more_results(_) ->
    false.

-spec sqlstate(reason()) -> binary().
sqlstate(#reason{state=S}) -> S.

-spec warning_count(result()) -> non_neg_integer()|undefined.
warning_count(#result{warning_count=W}) -> W;
warning_count(_) -> undefined.

-spec stmt_affected_rows(prepare()) -> non_neg_integer().
stmt_affected_rows(#prepare{result=R}) -> affected_rows(R).

-spec stmt_field_count(prepare()) -> non_neg_integer().
stmt_field_count(#prepare{field_count=F}) -> F.

-spec stmt_insert_id(prepare()) -> non_neg_integer().
stmt_insert_id(#prepare{result=R}) -> insert_id(R).

-spec stmt_param_count(prepare()) -> non_neg_integer().
stmt_param_count(#prepare{param_count=P}) -> P.

-spec stmt_attr_get(prepare(),non_neg_integer()) -> non_neg_integer().
stmt_attr_get(#prepare{flags=F}, ?STMT_ATTR_CURSOR_TYPE) -> F;
stmt_attr_get(#prepare{prefetch_rows=P}, ?STMT_ATTR_PREFETCH_ROWS) -> P.

-spec stmt_attr_set(prepare(),non_neg_integer(),non_neg_integer()) -> prepare().
stmt_attr_set(#prepare{}=X, ?STMT_ATTR_CURSOR_TYPE, Value) ->
    X#prepare{flags = Value};
stmt_attr_set(#prepare{}=X, ?STMT_ATTR_PREFETCH_ROWS, Value) ->
    X#prepare{prefetch_rows = Value}.

-spec stmt_warning_count(prepare()) -> non_neg_integer().
stmt_warning_count(#prepare{result=R}) -> warning_count(R).

%% == private ==

call(Pid, Command) ->
    case myer_client:call(Pid, Command) of
        {ok, {Fields,Rows,Record}} ->
            {ok, Fields, Rows, Record};
        {ok, undefined} ->
            ok;
        {ok, Record} ->
            {ok, Record};
        {error, Reason} ->
            {error, Reason}
    end.

%% mysql_affected_rows mysql_autocommit mysql_close mysql_commit
%% mysql_errno mysql_error mysql_get_server_version mysql_insert_id
%% mysql_ping mysql_real_connect mysql_real_query mysql_refresh
%% mysql_rollback mysql_select_db mysql_sqlstate mysql_warning_count
%% mysql_stmt_attr_get mysql_stmt_attr_set mysql_stmt_close
%% mysql_stmt_errno mysql_stmt_error mysql_stmt_execute
%% mysql_stmt_fetch mysql_stmt_field_count mysql_stmt_insert_id
%% mysql_stmt_param_count mysql_stmt_prepare mysql_stmt_reset
%% mysql_stmt_sqlstate mysql_stat mysql_stmt_send_long_data
%% mysql_more_results mysql_next_result

%% mysql_stmt_next_result
%% mysql_escape_string mysql_real_escape_string
%% mysql_set_local_infile_default mysql_set_local_infile_handler

%% mysql_character_set_name mysql_data_seek mysql_debug mysql_dump_debug_info
%% mysql_change_user mysql_fetch_field mysql_fetch_field_direct
%% mysql_fetch_fields mysql_fetch_lengths mysql_fetch_row mysql_field_count
%% mysql_field_seek mysql_field_tell mysql_free_result
%% mysql_get_character_set_info mysql_get_client_info
%% mysql_get_client_version mysql_get_host_info mysql_get_proto_info
%% mysql_get_server_info mysql_get_ssl_cipher mysql_info mysql_init
%% mysql_kill mysql_library_end mysql_library_init mysql_hex_string
%% mysql_list_dbs mysql_list_fields mysql_list_processes mysql_list_tables
%% mysql_num_fields mysql_num_rows mysql_row_seek mysql_row_tell
%% mysql_options mysql_options4 mysql_set_server_option
%% mysql_set_character_set mysql_shutdown mysql_ssl_set
%% mysql_store_result mysql_thread_id mysql_use_result
%% mysql_stmt_free_result mysql_stmt_init mysql_stmt_store_result
%% mysql_stmt_bind_param mysql_stmt_bind_result mysql_stmt_data_seek
%% mysql_stmt_fetch_column mysql_stmt_num_rows mysql_stmt_param_metadata
%% mysql_stmt_result_metadata mysql_stmt_row_seek mysql_stmt_row_tell

%% mysql_connect mysql_create_db mysql_drop_db mysql_eof mysql_query
%% mysql_reload
