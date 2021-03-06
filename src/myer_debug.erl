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

-module(myer_debug).

-include("internal.hrl").

%% -- public --
-export([stat/1, caps/1, flags/1, type/1]).

%% -- internal --
-define(BCHECK(B,K,V), (case B band K of 0 -> ""; _ -> V end)).

%% == public ==

-spec stat(integer()|result()) -> string().
stat(S)
  when is_integer(S) ->
    L = [
         ?BCHECK(S,?SERVER_STATUS_IN_TRANS, "SERVER_STATUS_IN_TRANS"),
         ?BCHECK(S,?SERVER_STATUS_AUTOCOMMIT, "SERVER_STATUS_AUTOCOMMIT"),
         ?BCHECK(S,?SERVER_MORE_RESULTS_EXISTS, "SERVER_MORE_RESULTS_EXISTS"),
         ?BCHECK(S,?SERVER_QUERY_NO_GOOD_INDEX_USED, "SERVER_QUERY_NO_GOOD_INDEX_USED"),
         ?BCHECK(S,?SERVER_QUERY_NO_INDEX_USED, "SERVER_QUERY_NO_INDEX_USED"),
         ?BCHECK(S,?SERVER_STATUS_CURSOR_EXISTS, "SERVER_STATUS_CURSOR_EXISTS"),
         ?BCHECK(S,?SERVER_STATUS_LAST_ROW_SENT, "SERVER_STATUS_LAST_ROW_SENT"),
         ?BCHECK(S,?SERVER_STATUS_DB_DROPPED, "SERVER_STATUS_DB_DROPPED"),
         ?BCHECK(S,?SERVER_STATUS_NO_BACKSLASH_ESCAPES, "SERVER_STATUS_NO_BACKSLASH_ESCAPES"),
         ?BCHECK(S,?SERVER_STATUS_METADATA_CHANGED, "SERVER_STATUS_METADATA_CHANGED"),
         ?BCHECK(S,?SERVER_QUERY_WAS_SLOW, "SERVER_QUERY_WAS_SLOW"),
         ?BCHECK(S,?SERVER_PS_OUT_PARAMS, "SERVER_PS_OUT_PARAMS"),
         ?BCHECK(S,?SERVER_STATUS_IN_TRANS_READONLY, "SERVER_STATUS_IN_TRANS_READONLY"),
         ?BCHECK(S,?SERVER_SESSION_STATE_CHANGED, "SERVER_SESSION_STATE_CHANGED")
        ],
    string:join([ E || E <- L, length(E) > 0 ], ",");
stat(#result{status=S}) ->
    stat(S);
stat(_) ->
    "?".

-spec caps(integer()) -> string().
caps(C)
  when is_integer(C) ->
    L = [
         ?BCHECK(C,?CLIENT_LONG_PASSWORD, "CLIENT_LONG_PASSWORD"),
         ?BCHECK(C,?CLIENT_FOUND_ROWS, "CLIENT_FOUND_ROWS"),
         ?BCHECK(C,?CLIENT_LONG_FLAG, "CLIENT_LONG_FLAG"),
         ?BCHECK(C,?CLIENT_CONNECT_WITH_DB, "CLIENT_CONNECT_WITH_DB"),
         ?BCHECK(C,?CLIENT_NO_SCHEMA, "CLIENT_NO_SCHEMA"),
         ?BCHECK(C,?CLIENT_COMPRESS, "CLIENT_COMPRESS"),
         ?BCHECK(C,?CLIENT_ODBC, "CLIENT_ODBC"),
         ?BCHECK(C,?CLIENT_LOCAL_FILES, "CLIENT_LOCAL_FILES"),
         ?BCHECK(C,?CLIENT_IGNORE_SPACE, "CLIENT_IGNORE_SPACE"),
         ?BCHECK(C,?CLIENT_PROTOCOL_41, "CLIENT_PROTOCOL_41"),
         ?BCHECK(C,?CLIENT_INTERACTIVE, "CLIENT_INTERACTIVE"),
         ?BCHECK(C,?CLIENT_SSL, "CLIENT_SSL"),
         ?BCHECK(C,?CLIENT_IGNORE_SIGPIPE, "CLIENT_IGNORE_SIGPIPE"),
         ?BCHECK(C,?CLIENT_TRANSACTIONS, "CLIENT_TRANSACTIONS"),
         ?BCHECK(C,?CLIENT_RESERVED, "CLIENT_RESERVED"),
         ?BCHECK(C,?CLIENT_SECURE_CONNECTION, "CLIENT_SECURE_CONNECTION"),
         ?BCHECK(C,?CLIENT_MULTI_STATEMENTS, "CLIENT_MULTI_STATEMENTS"),
         ?BCHECK(C,?CLIENT_MULTI_RESULTS, "CLIENT_MULTI_RESULTS"),
         ?BCHECK(C,?CLIENT_PS_MULTI_RESULTS, "CLIENT_PS_MULTI_RESULTS"),
         ?BCHECK(C,?CLIENT_PLUGIN_AUTH, "CLIENT_PLUGIN_AUTH"),
         ?BCHECK(C,?CLIENT_CONNECT_ATTRS, "CLIENT_CONNECT_ATTRS"),
         ?BCHECK(C,?CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA, "CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA"),
         ?BCHECK(C,?CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS, "CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS"),
         ?BCHECK(C,?CLIENT_SESSION_TRACK, "CLIENT_SESSION_TRACK"),
         ?BCHECK(C,?CLIENT_DEPRECATE_EOF, "CLIENT_DEPRECATE_EOF"),
         ?BCHECK(C,?CLIENT_SSL_VERIFY_SERVER_CERT, "CLIENT_SSL_VERIFY_SERVER_CERT"),
         ?BCHECK(C,?CLIENT_REMEMBER_OPTIONS, "CLIENT_REMEMBER_OPTIONS")
        ],
    string:join([ E || E <- L, length(E) > 0 ], ",");
caps(_) ->
    "?".

-spec flags(integer()|field()) -> string().
flags(C)
  when is_integer(C) ->
    L = [
         ?BCHECK(C,?NOT_NULL_FLAG, "NOT_NULL_FLAG"),
         ?BCHECK(C,?PRI_KEY_FLAG, "PRI_KEY_FLAG"),
         ?BCHECK(C,?UNIQUE_KEY_FLAG, "UNIQUE_KEY_FLAG"),
         ?BCHECK(C,?MULTIPLE_KEY_FLAG, "MULTIPLE_KEY_FLAG"),
         ?BCHECK(C,?BLOB_FLAG, "BLOB_FLAG"),
         ?BCHECK(C,?UNSIGNED_FLAG, "UNSIGNED_FLAG"),
         ?BCHECK(C,?ZEROFILL_FLAG, "ZEROFILL_FLAG"),
         ?BCHECK(C,?BINARY_FLAG, "BINARY_FLAG"),
         ?BCHECK(C,?ENUM_FLAG, "ENUM_FLAG"),
         ?BCHECK(C,?AUTO_INCREMENT_FLAG, "AUTO_INCREMENT_FLAG"),
         ?BCHECK(C,?TIMESTAMP_FLAG, "TIMESTAMP_FLAG"),
         ?BCHECK(C,?SET_FLAG, "SET_FLAG"),
         ?BCHECK(C,?NO_DEFAULT_VALUE_FLAG, "NO_DEFAULT_VALUE_FLAG"),
         ?BCHECK(C,?ON_UPDATE_NOW_FLAG, "ON_UPDATE_NOW_FLAG"),
         ?BCHECK(C,?PART_KEY_FLAG, "PART_KEY_FLAG"),
         ?BCHECK(C,?GROUP_FLAG, "GROUP_FLAG"),
         ?BCHECK(C,?UNIQUE_FLAG, "UNIQUE_FLAG"),
         ?BCHECK(C,?BINCMP_FLAG, "BINCMP_FLAG"),
         ?BCHECK(C,?GET_FIXED_FIELDS_FLAG, "GET_FIXED_FIELDS_FLAG"),
         ?BCHECK(C,?FIELD_IN_PART_FUNC_FLAG, "FIELD_IN_PART_FUNC_FLAG")
        ],
    string:join([ E || E <- L, length(E) > 0 ], ",");
flags(#field{flags=F}) ->
    flags(F);
flags(_) ->
    "?".

-spec type(integer()|field()) -> string().
type(T)
  when is_integer(T), T > 0 ->
    L = [
         {?MYSQL_TYPE_DECIMAL, "MYSQL_TYPE_DECIMAL"},
         {?MYSQL_TYPE_TINY, "MYSQL_TYPE_TINY"},
         {?MYSQL_TYPE_SHORT, "MYSQL_TYPE_SHORT"},
         {?MYSQL_TYPE_LONG, "MYSQL_TYPE_LONG"},
         {?MYSQL_TYPE_FLOAT, "MYSQL_TYPE_FLOAT"},
         {?MYSQL_TYPE_DOUBLE, "MYSQL_TYPE_DOUBLE"},
         {?MYSQL_TYPE_NULL, "MYSQL_TYPE_NULL"},
         {?MYSQL_TYPE_TIMESTAMP, "MYSQL_TYPE_TIMESTAMP"},
         {?MYSQL_TYPE_LONGLONG, "MYSQL_TYPE_LONGLONG"},
         {?MYSQL_TYPE_INT24, "MYSQL_TYPE_INT24"},
         {?MYSQL_TYPE_DATE, "MYSQL_TYPE_DATE"},
         {?MYSQL_TYPE_TIME, "MYSQL_TYPE_TIME"},
         {?MYSQL_TYPE_DATETIME, "MYSQL_TYPE_DATETIME"},
         {?MYSQL_TYPE_YEAR, "MYSQL_TYPE_YEAR"},
         {?MYSQL_TYPE_NEWDATE, "MYSQL_TYPE_NEWDATE"},
         {?MYSQL_TYPE_VARCHAR, "MYSQL_TYPE_VARCHAR"},
         {?MYSQL_TYPE_BIT, "MYSQL_TYPE_BIT"},
         {?MYSQL_TYPE_TIMESTAMP2, "MYSQL_TYPE_TIMESTAMP2"},
         {?MYSQL_TYPE_DATETIME2, "MYSQL_TYPE_DATETIME2"},
         {?MYSQL_TYPE_TIME2, "MYSQL_TYPE_TIME2"},
         {?MYSQL_TYPE_JSON, "MYSQL_TYPE_JSON"},
         {?MYSQL_TYPE_NEWDECIMAL, "MYSQL_TYPE_NEWDECIMAL"},
         {?MYSQL_TYPE_ENUM, "MYSQL_TYPE_ENUM"},
         {?MYSQL_TYPE_SET, "MYSQL_TYPE_SET"},
         {?MYSQL_TYPE_TINY_BLOB, "MYSQL_TYPE_TINY_BLOB"},
         {?MYSQL_TYPE_MEDIUM_BLOB, "MYSQL_TYPE_MEDIUM_BLOB"},
         {?MYSQL_TYPE_LONG_BLOB, "MYSQL_TYPE_LONG_BLOB"},
         {?MYSQL_TYPE_BLOB, "MYSQL_TYPE_BLOB"},
         {?MYSQL_TYPE_VAR_STRING, "MYSQL_TYPE_VAR_STRING"},
         {?MYSQL_TYPE_STRING, "MYSQL_TYPE_STRING"},
         {?MYSQL_TYPE_GEOMETRY, "MYSQL_TYPE_GEOMETRY"}
        ],
    proplists:get_value(T, L, "unknown");
type(#field{type=T}) ->
    type(T);
type(_) ->
    "?".
