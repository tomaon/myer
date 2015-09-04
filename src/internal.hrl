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

-include_lib("baseline/include/baseline.hrl").

-include("../include/myer.hrl").

%% == define ==

%% @see ~/include/mysql_com.h

%% -- define --
-define(MAX_PACKET_LENGTH, 16#FFFFFF). % 16777215
-define(MIN_PACKET_LENGTH, 16#00FFFF). %    65535, TODO

%%efine(NET_READ_TIMEOUT,         30).
%%efine(NET_WRITE_TIMEOUT,        60).
-define(NET_WAIT_TIMEOUT,       3000). % 8*60*60

-define(SERVER_STATUS_IN_TRANS,                 (1 bsl  0)).
-define(SERVER_STATUS_AUTOCOMMIT,               (1 bsl  1)).
-define(SERVER_MORE_RESULTS_EXISTS,             (1 bsl  3)).
-define(SERVER_QUERY_NO_GOOD_INDEX_USED,        (1 bsl  4)).
-define(SERVER_QUERY_NO_INDEX_USED,             (1 bsl  5)).
-define(SERVER_STATUS_CURSOR_EXISTS,            (1 bsl  6)).
-define(SERVER_STATUS_LAST_ROW_SENT,            (1 bsl  7)).
-define(SERVER_STATUS_DB_DROPPED,               (1 bsl  8)).
-define(SERVER_STATUS_NO_BACKSLASH_ESCAPES,     (1 bsl  9)).
-define(SERVER_STATUS_METADATA_CHANGED,         (1 bsl 10)).
-define(SERVER_QUERY_WAS_SLOW,                  (1 bsl 11)).
-define(SERVER_PS_OUT_PARAMS,                   (1 bsl 12)).
-define(SERVER_STATUS_IN_TRANS_READONLY,        (1 bsl 13)). % > 5.6
-define(SERVER_SESSION_STATE_CHANGED,           (1 bsl 14)). % > 5.7

-define(CLIENT_LONG_PASSWORD,                   (1 bsl  0)).
-define(CLIENT_FOUND_ROWS,                      (1 bsl  1)).
-define(CLIENT_LONG_FLAG,                       (1 bsl  2)).
-define(CLIENT_CONNECT_WITH_DB,                 (1 bsl  3)).
-define(CLIENT_NO_SCHEMA,                       (1 bsl  4)).
-define(CLIENT_COMPRESS,                        (1 bsl  5)).
-define(CLIENT_ODBC,                            (1 bsl  6)).
-define(CLIENT_LOCAL_FILES,                     (1 bsl  7)).
-define(CLIENT_IGNORE_SPACE,                    (1 bsl  8)).
-define(CLIENT_PROTOCOL_41,                     (1 bsl  9)).
-define(CLIENT_INTERACTIVE,                     (1 bsl 10)).
-define(CLIENT_SSL,                             (1 bsl 11)).
-define(CLIENT_IGNORE_SIGPIPE,                  (1 bsl 12)).
-define(CLIENT_TRANSACTIONS,                    (1 bsl 13)).
-define(CLIENT_RESERVED,                        (1 bsl 14)).
-define(CLIENT_SECURE_CONNECTION,               (1 bsl 15)). % > 5.7, CLIENT_RESERVED2
-define(CLIENT_MULTI_STATEMENTS,                (1 bsl 16)).
-define(CLIENT_MULTI_RESULTS,                   (1 bsl 17)).
-define(CLIENT_PS_MULTI_RESULTS,                (1 bsl 18)).
-define(CLIENT_PLUGIN_AUTH,                     (1 bsl 19)).
-define(CLIENT_CONNECT_ATTRS,                   (1 bsl 20)). % > 5.6
-define(CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA,  (1 bsl 21)). % > 5.6
-define(CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS,    (1 bsl 22)). % > 5.6
-define(CLIENT_SESSION_TRACK,                   (1 bsl 23)). % > 5.7
-define(CLIENT_DEPRECATE_EOF,                   (1 bsl 24)). % > 5.7
-define(CLIENT_SSL_VERIFY_SERVER_CERT,          (1 bsl 30)).
-define(CLIENT_REMEMBER_OPTIONS,                (1 bsl 31)).

%% -- enum: enum_server_command --
%%efine(COM_SLEEP,                0). % internal
-define(COM_QUIT,                 1).
-define(COM_INIT_DB,              2).
-define(COM_QUERY,                3).
%%efine(COM_FIELD_LIST,           4).
%%efine(COM_CREATE_DB,            5). % deprecated
%%efine(COM_DROP_DB,              6). % deprecated
-define(COM_REFRESH,              7).
%%efine(COM_SHUTDOWN,             8).
-define(COM_STATISTICS,           9).
%%efine(COM_PROCESS_INFO,        10).
%%efine(COM_CONNECT,             11). % internal
%%efine(COM_PROCESS_KILL,        12).
%%efine(COM_DEBUG,               13).
-define(COM_PING,                14).
%%efine(COM_TIME,                15). % internal
%%efine(COM_DELAYED_INSERT,      16). % internal
%%efine(COM_CHANGE_USER,         17).
%%efine(COM_BINLOG_DUMP,         18). % internal
%%efine(COM_TABLE_DUMP,          19). % internal
%%efine(COM_CONNECT_OUT,         20). % internal
%%efine(COM_REGISTER_SLAVE,      21). % internal?
-define(COM_STMT_PREPARE,        22).
-define(COM_STMT_EXECUTE,        23).
-define(COM_STMT_SEND_LONG_DATA, 24).
-define(COM_STMT_CLOSE,          25).
-define(COM_STMT_RESET,          26).
%%efine(COM_SET_OPTION,          27).
-define(COM_STMT_FETCH,          28).
%%efine(COM_DAEMON,              29).
%%efine(COM_BINLOG_DUMP_GTID,    30). % > 5.6
%%efine(COM_RESET_CONNECTION,    31). % > 5.7

%% (> 5.7, @see ~/include/mysql.h)

%% -- enum: enum_field_types --
-define(MYSQL_TYPE_DECIMAL,       0). %
-define(MYSQL_TYPE_TINY,          1). % TINYINT
-define(MYSQL_TYPE_SHORT,         2). % SMALLINT
-define(MYSQL_TYPE_LONG,          3). % INT,INTEGER
-define(MYSQL_TYPE_FLOAT,         4). % FLOAT
-define(MYSQL_TYPE_DOUBLE,        5). % DOUBLE,REAL
-define(MYSQL_TYPE_NULL,          6). %
-define(MYSQL_TYPE_TIMESTAMP,     7). % TIMESTAMP
-define(MYSQL_TYPE_LONGLONG,      8). % BIGINT
-define(MYSQL_TYPE_INT24,         9). % MEDIUMINT
-define(MYSQL_TYPE_DATE,         10). % DATE
-define(MYSQL_TYPE_TIME,         11). % TIME
-define(MYSQL_TYPE_DATETIME,     12). % DATETIME
-define(MYSQL_TYPE_YEAR,         13). % YEAR
-define(MYSQL_TYPE_NEWDATE,      14). %
-define(MYSQL_TYPE_VARCHAR,      15). %
-define(MYSQL_TYPE_BIT,          16). % BIT
-define(MYSQL_TYPE_TIMESTAMP2,   17). % > 5.6, TIMESTAMP
-define(MYSQL_TYPE_DATETIME2,    18). % > 5.6, DATETIME
-define(MYSQL_TYPE_TIME2,        19). % > 5.6, TIME
-define(MYSQL_TYPE_JSON,        245). % > 5.7, JSON
-define(MYSQL_TYPE_NEWDECIMAL,  246). % DECIMAL,NUMERIC
-define(MYSQL_TYPE_ENUM,        247). %
-define(MYSQL_TYPE_SET,         248). %
-define(MYSQL_TYPE_TINY_BLOB,   249). %
-define(MYSQL_TYPE_MEDIUM_BLOB, 250). %
-define(MYSQL_TYPE_LONG_BLOB,   251). %
-define(MYSQL_TYPE_BLOB,        252). % *BLOB,*TEXT
-define(MYSQL_TYPE_VAR_STRING,  253). % VARCHAR,VARBINARY,ENUM,SET
-define(MYSQL_TYPE_STRING,      254). % CHAR,BINARY
-define(MYSQL_TYPE_GEOMETRY,    255). %

%% == record ==

-record(result, {
          affected_rows :: non_neg_integer(),
          insert_id :: non_neg_integer(),
          status :: integer(),
          warning_count :: non_neg_integer(),
          message :: binary()
         }).

-type(result() :: #result{}).

-record(reason, {
          errno :: non_neg_integer(),
          reserved :: binary(),
          state :: binary(),
          message :: binary()
         }).

-type(reason() :: #reason{}).

-record(protocol, {
          handle :: tuple(),
          caps :: integer()
         }).

-type(protocol() :: #protocol{}).

-record(prepare, {
          stmt_id :: non_neg_integer(),
          field_count :: non_neg_integer(),
          fields :: list(),
          param_count :: non_neg_integer(),
          params :: list(),
          warning_count :: non_neg_integer(),
          reserved :: binary(),
          flags :: non_neg_integer(),
          prefetch_rows :: non_neg_integer(),
          result :: tuple(),
          execute :: non_neg_integer()
         }).

-type(prepare() :: #prepare{}).

-record(handshake, {
          version :: [non_neg_integer()],
          tid :: non_neg_integer(),
          seed1 :: binary(),
          caps1 :: non_neg_integer(),
          charset :: non_neg_integer(),
          status :: non_neg_integer(),
          caps2 :: non_neg_integer(),
          length :: non_neg_integer(),
          reserved :: binary(),
          seed2 :: binary(),
          plugin :: binary(),

          maxlength :: non_neg_integer(),
          seed :: binary(),
          caps :: integer()
         }).

-type(handshake() :: #handshake{}).

-record(plugin, {
          name :: binary()
         }).

-type(plugin() :: #plugin{}).

-record(handle, {
          module        :: module(),            % baseline_socket
          socket        :: tuple(),             % baseline_socket:socket()
          maxlength     :: non_neg_integer(),
          timeout       :: timeout(),

          caps          :: non_neg_integer(),
          version       :: [non_neg_integer()],

          seqnum = 0    :: non_neg_integer(),
          buf    = <<>> :: binary(),
          start  = 0    :: non_neg_integer(),
          length = 0    :: non_neg_integer(),

          zseqnum = 0 :: non_neg_integer(),
          zraw = <<>> :: binary()
         }).

-type(handle() :: #handle{}).

%% == type ==

-type(properties() :: [{atom(),term()}]).
