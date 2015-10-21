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
-export([real_query/2, next_result/1]).
-export([autocommit/2, commit/1, rollback/1]).
-export([prepare/3, unprepare/2, execute/3]).

%% -- public: record --
-export([affected_rows/1, errno/1, errmsg/1, insert_id/1, more_results/1, sqlstate/1,
         warning_count/1]).

%% -- internal --
-record(myer, {
          sup     :: pid(),
          worker  :: pid(),
          timeout :: timeout()
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
                    {ok, #myer{sup = Sup, worker = Pid,
                               timeout = timer:seconds(?NET_READ_TIMEOUT)}};
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
get_server_version(#myer{worker=W,timeout=T}) ->
    myer_client:version(W, T).

-spec stat(myer()) -> {ok,binary()}|{error,_}.
stat(#myer{worker=W,timeout=T}) ->
    myer_client:stat(W, T).


-spec ping(myer()) -> {ok,result()}|{error,_}.
ping(#myer{worker=W,timeout=T}) ->
    myer_client:ping(W, T).

-spec refresh(myer(),integer()) -> {ok,result()}|{error,_}.
refresh(#myer{worker=W,timeout=T}, Option) ->
    myer_client:refresh(W, Option, T).

-spec select_db(myer(),binary()) -> {ok,result()}|{error,_}.
select_db(#myer{worker=W,timeout=T}, Database) ->
    myer_client:select_db(W, Database, T).


-spec real_query(myer(),binary()) ->
                        {ok,result()}|
                        {ok,fields(),rows(),result()}|
                        {error,_}.
real_query(#myer{worker=W,timeout=T}, Query) ->
    myer_client:real_query(W, Query, T).

-spec next_result(myer()) ->
                         {ok,result()}|
                         {ok,fields(),rows(),result()}|
                         {error,_}.
next_result(#myer{worker=W,timeout=T})
  when is_pid(W) ->
    myer_client:next_result(W, T).


-spec autocommit(myer(),boolean()) -> {ok,result()}|{error,_}.
autocommit(#myer{worker=W,timeout=T}, Boolean) ->
    myer_client:autocommit(W, Boolean, T).

-spec commit(myer()) -> {ok,result()}|{error,_}.
commit(#myer{worker=W,timeout=T}) ->
    myer_client:commit(W, T).

-spec rollback(myer()) -> {ok,result()}|{error,_}.
rollback(#myer{worker=W,timeout=T}) ->
    myer_client:rollback(W, T).


-spec prepare(myer(),binary(),binary()) -> {ok,result()}|{error,_}.
prepare(#myer{worker=W,timeout=T}, Name, Query) ->
    myer_client:prepare(W, Name, Query, T).

-spec unprepare(myer(),binary()) -> {ok,result()}|{error,_}.
unprepare(#myer{worker=W,timeout=T}, Name) ->
    myer_client:unprepare(W, Name, T).

-spec execute(myer(),binary(),params()) ->
                     {ok,result()}|
                     {ok,fields(),rows(),result()}|
                     {error,_}.
execute(#myer{worker=W,timeout=T}, Name, Params) ->
    myer_client:execute(W, Name, Params, T).

%% == public: record ==

-spec affected_rows(term()) -> non_neg_integer()|undefined.
affected_rows(Term) -> myer_client:affected_rows(Term).

-spec errno(term()) -> non_neg_integer()|undefined.
errno(Term) -> myer_client:errno(Term).

-spec errmsg(term()) -> binary()|undefined.
errmsg(Term) -> myer_client:errmsg(Term).

-spec insert_id(term()) -> non_neg_integer()|undefined.
insert_id(Term) -> myer_client:insert_id(Term).

-spec more_results(term()) -> boolean().
more_results(Term) -> myer_client:more_results(Term).

-spec sqlstate(term()) -> binary()|undefined.
sqlstate(Term) -> myer_client:sqlstate(Term).

-spec warning_count(term()) -> non_neg_integer()|undefined.
warning_count(Term) -> myer_client:warning_count(Term).

%% mysql_affected_rows mysql_autocommit mysql_close mysql_commit
%% mysql_errno mysql_error mysql_get_server_version mysql_insert_id
%% mysql_ping mysql_real_connect mysql_real_query mysql_refresh
%% mysql_rollback mysql_select_db mysql_sqlstate mysql_warning_count
%% mysql_more_results mysql_next_result

%% mysql_stmt_attr_get mysql_stmt_attr_set mysql_stmt_close
%% mysql_stmt_errno mysql_stmt_error mysql_stmt_execute
%% mysql_stmt_fetch mysql_stmt_field_count mysql_stmt_insert_id
%% mysql_stmt_param_count mysql_stmt_prepare mysql_stmt_reset
%% mysql_stmt_sqlstate mysql_stat mysql_stmt_send_long_data
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
