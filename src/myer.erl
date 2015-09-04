%% =============================================================================
%% Copyright 2013-2015 AONO Tomohiko
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License version 2.1 as published by the Free Software Foundation.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%% =============================================================================

-module(myer).

-include("internal.hrl").

%% -- public: application --
-export([start/0, start/1, stop/0, version/0]).

%% -- public: pool --
-export([checkout/1, checkin/1]).
-export([set_timeout/2]).

%% -- public: worker --
-export([get_server_version/1, stat/1]).
-export([ping/1, refresh/2, select_db/2]).
-export([real_query/2, autocommit/2, commit/1, rollback/1 ]).

-export([next_result/1]).
-export([stmt_close/2, stmt_execute/3, stmt_fetch/2, stmt_prepare/2, stmt_reset/2,
         stmt_next_result/2]).

%% -- public: record --
-export([affected_rows/1, errno/1, errmsg/1, insert_id/1, more_results/1, sqlstate/1,
         warning_count/1]).
-export([stmt_affected_rows/1, stmt_field_count/1, stmt_insert_id/1, stmt_param_count/1,
         stmt_attr_get/2, stmt_attr_set/3, stmt_warning_count/1]).

%% -- internal --
-record(myer, {
          sup :: pid(),
          worker :: pid(),
          timeout = ?NET_WAIT_TIMEOUT :: timeout()
         }).

-type(myer() :: #myer{}).

%% == public: application ==

-spec start() -> ok|{error,_}.
start() ->
    start(temporary).

-spec start(atom()) -> ok|{error,_}.
start(Type)
  when is_atom(Type) ->
    baseline_app:ensure_start(?MODULE, Type).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec version() -> [term()].
version() ->
    baseline_app:version(?MODULE).

%% == public: pool ==

-spec checkout(atom()) -> {ok,myer()}|{error,_}.
checkout(Pool)
  when is_atom(Pool) ->
    case baseline_sup:find(myer_sup, Pool) of
        undefined ->
            {error,notfound};
        Sup ->
            case supervisor:start_child(Sup, []) of
                {ok, Pid} ->
                    true = link(Pid),
                    {ok, #myer{sup = Sup, worker = Pid}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec checkin(myer()) -> ok|{error,not_found|simple_one_for_one}.
checkin(#myer{sup=S,worker=W})
  when is_pid(S), is_pid(W) ->
    true = unlink(W),
    supervisor:terminate_child(S, W).


-spec set_timeout(myer(),timeout()) -> {ok,myer()}.
set_timeout(#myer{}=H, Timeout)
  when ?IS_TIMEOUT(Timeout) ->
    {ok, H#myer{timeout = Timeout}}.

%% == public: worker ==

-spec get_server_version(myer()) -> {ok,[non_neg_integer()]}|{error,_}.
get_server_version(#myer{worker=W,timeout=T})
  when is_pid(W) ->
    myer_client:version(W, T).

-spec stat(myer()) -> {ok,binary()}|{error,_}.
stat(#myer{worker=W,timeout=T})
  when is_pid(W) ->
    myer_client:stat(W, T).


-spec ping(myer()) -> {ok,result()}|{error,_}.
ping(#myer{worker=W,timeout=T})
  when is_pid(W) ->
    myer_client:ping(W, T).

-spec refresh(myer(),integer()) -> {ok,result()}|{error,_}.
refresh(#myer{worker=W,timeout=T}, Option)
  when is_pid(W), is_integer(Option) ->
    myer_client:refresh(W, Option, T).

-spec select_db(myer(),binary()) -> {ok,result()}|{error,_}.
select_db(#myer{worker=W,timeout=T}, Database)
  when is_pid(W), is_binary(Database) ->
    myer_client:select_db(W, Database, T).


-spec real_query(myer(),binary()) ->
                        {ok,result()}|
                        {ok,fields(),rows(),result()}|
                        {error,_}.
real_query(#myer{worker=W,timeout=T}, Query)
  when is_pid(W), is_binary(Query) ->
    myer_client:real_query(W, Query, T).

-spec autocommit(myer(),boolean()) -> {ok,result()}|{error,_}.
autocommit(#myer{worker=W,timeout=T}, Boolean)
  when is_pid(W), is_boolean(Boolean) ->
    myer_client:autocommit(W, Boolean, T).

-spec commit(myer()) -> {ok,result()}|{error,_}.
commit(#myer{worker=W,timeout=T})
  when is_pid(W) ->
    myer_client:commit(W, T).

-spec rollback(myer()) -> {ok,result()}|{error,_}.
rollback(#myer{worker=W,timeout=T})
  when is_pid(W) ->
    myer_client:rollback(W, T).


-spec next_result(myer()) -> {ok,result()}|{ok,[field()],[term()],result()}|{error,_}.
next_result(#myer{worker=W})
  when is_pid(W) ->
    myer_client:call(W, {next_result,[]}).

-spec stmt_close(myer(),prepare()) -> ok|{error,_}.
stmt_close(#myer{worker=W}, #prepare{}=X)
  when is_pid(W) ->
    myer_client:call(W, {stmt_close,[X]}).

-spec stmt_execute(myer(),prepare(),[term()])
                  -> {ok,prepare()}|{ok,[field()],[term()],prepare()}|{error,_}.
stmt_execute(#myer{worker=W}, #prepare{param_count=N}=X, Args)
  when is_pid(W), is_list(Args), N == length(Args) ->
    myer_client:call(W, {stmt_execute,[X,Args]}).

-spec stmt_fetch(myer(),prepare())
                -> {ok,prepare()}|
                   {ok,[field()],[term()],prepare()}|{error,_}.
stmt_fetch(#myer{worker=W}, #prepare{}=X)
  when is_pid(W) ->
    myer_client:call(W, {stmt_fetch,[X]}).

-spec stmt_prepare(myer(),binary()) -> {ok,prepare()}|{error,_}.
stmt_prepare(#myer{worker=W}, Query)
  when is_pid(W), is_binary(Query) ->
    myer_client:call(W, {stmt_prepare,[Query]}).

-spec stmt_reset(myer(),prepare()) -> {ok,prepare()}|{error,_}.
stmt_reset(#myer{worker=W}, #prepare{}=X)
  when is_pid(W) ->
    myer_client:call(W, {stmt_reset,[X]}).

-spec stmt_next_result(myer(),prepare())
                      -> {ok,prepare()}|{ok,[field()],[term()],prepare()}|{error,_}.
stmt_next_result(#myer{worker=W}, #prepare{}=X)
  when is_pid(W) ->
    myer_client:call(W, {stmt_next_result,[X]}).

%% stmt_send_long_data, TODO

%% == public: record ==

-spec affected_rows(term()) -> non_neg_integer()|undefined.
affected_rows(#result{affected_rows=A}) -> A;
affected_rows(_) -> undefined.

-spec errno(term()) -> non_neg_integer()|undefined.
errno(#reason{errno=E}) -> E;
errno(_) -> undefined.

-spec errmsg(term()) -> binary()|undefined.
errmsg(#reason{message=M}) -> M; % rename error/1 to errmsg/1
errmsg(_) -> undefined.

-spec insert_id(term()) -> non_neg_integer()|undefined.
insert_id(#result{insert_id=I}) -> I;
insert_id(_) -> undefined.

-spec more_results(term()) -> boolean().
more_results(#prepare{result=R}) ->
    more_results(R);
more_results(#result{status=S})
  when ?IS_SET(S,?SERVER_MORE_RESULTS_EXISTS) ->
    true;
more_results(_) ->
    false.

-spec sqlstate(term()) -> binary()|undefined.
sqlstate(#reason{state=S}) -> S;
sqlstate(_) -> undefined.

-spec warning_count(term()) -> non_neg_integer()|undefined.
warning_count(#result{warning_count=W}) -> W;
warning_count(_) -> undefined.

-spec stmt_affected_rows(term()) -> non_neg_integer()|undefined.
stmt_affected_rows(#prepare{result=R}) -> affected_rows(R);
stmt_affected_rows(_) -> undefined.

-spec stmt_field_count(term()) -> non_neg_integer()|undefined.
stmt_field_count(#prepare{field_count=F}) -> F;
stmt_field_count(_) -> undefined.

-spec stmt_insert_id(term()) -> non_neg_integer()|undefined.
stmt_insert_id(#prepare{result=R}) -> insert_id(R);
stmt_insert_id(_) -> undefined.

-spec stmt_param_count(term()) -> non_neg_integer()|undefined.
stmt_param_count(#prepare{param_count=P}) -> P;
stmt_param_count(_) -> undefined.

-spec stmt_attr_get(term(),non_neg_integer()) -> non_neg_integer()|undefined.
stmt_attr_get(#prepare{flags=F}, ?STMT_ATTR_CURSOR_TYPE) -> F;
stmt_attr_get(#prepare{prefetch_rows=P}, ?STMT_ATTR_PREFETCH_ROWS) -> P;
stmt_attr_get(_,_) -> undefined.

-spec stmt_attr_set(prepare(),non_neg_integer(),non_neg_integer()) -> prepare().
stmt_attr_set(#prepare{}=X, ?STMT_ATTR_CURSOR_TYPE, Value) ->
    X#prepare{flags = Value};
stmt_attr_set(#prepare{}=X, ?STMT_ATTR_PREFETCH_ROWS, Value) ->
    X#prepare{prefetch_rows = Value}.

-spec stmt_warning_count(term()) -> non_neg_integer()|undefined.
stmt_warning_count(#prepare{result=R}) -> warning_count(R);
stmt_warning_count(_) -> undefined.


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
