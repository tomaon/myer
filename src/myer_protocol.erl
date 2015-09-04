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

-module(myer_protocol).

-include("internal.hrl").

%% -- private --
-export([connect/1, close/1, auth/1]).
-export([stat/1, version/1]).
-export([ping/1, refresh/1, select_db/1]).
-export([real_query/1]).
-export([stmt_prepare/1, stmt_close/1, stmt_reset/1,
         stmt_execute/1, stmt_fetch/1]).
%%xport([next_result/1, stmt_next_result/2]).

%% -- internal --

-import(myer_handle, [recv_binary/2, recv_text/2,
                      remains/1, reset/1]).

-export([binary_to_float/2,
         recv_packed_binary/1, recv_packed_binary/2, recv_unsigned/2]).

-record(handshake, {
          version   :: version(),
          tid       :: non_neg_integer(),
          seed1     :: binary(),
          caps1     :: non_neg_integer(),
          charset   :: non_neg_integer(),
          status    :: non_neg_integer(),
          caps2     :: non_neg_integer(),
          length    :: non_neg_integer(),
          reserved  :: binary(),
          seed2     :: binary(),
          plugin    :: binary(),
          maxlength :: non_neg_integer(),
          seed      :: binary(),
          caps      :: integer()
         }).

-record(plugin, {
          name :: binary()
         }).

-type(args() :: [term()]).
-type(handle() :: myer_handle:handle()).
-type(handshake() :: #handshake{}).

%% == private ==

-spec connect(args()) -> {ok,handshake(),handle()}|{error,_}|{error,_,handle()}.
connect(Args) ->
    loop(Args, [fun connect_pre/5, fun connect_post/2]).

-spec close(args()) -> {ok,undefined}|{error,_,handle()}.
close(Args) ->
    loop(Args, [fun close_pre/1, fun send/2, fun close_post/1]).

-spec auth(args()) -> {ok,result(),handle()}|{error,_,handle()}.
auth(Args) ->
    case loop(Args, [fun auth_pre/6, fun send/3, fun auth_post/2]) of
        {ok, #plugin{name=N}, #handshake{}=H, Handle} ->
            auth_alt([Handle,lists:nth(3,Args),H#handshake{plugin = N}]);
        {ok, Result, Handle}->
            {ok, Result, Handle};
        {error, Reason, Handle} ->
            {error, Reason, Handle}
    end.

auth_alt(Args) ->
    loop(Args, [fun auth_alt_pre/3, fun send/2, fun auth_alt_post/1]).


-spec stat(args()) -> {ok,binary(),handle()}|{error,_,handle()}. % != result()
stat(Args) ->
    loop(Args, [fun stat_pre/1, fun send/2, fun stat_post/1]).

-spec version([handle()]) -> {ok,version(),handle()}. % != result()
version([Handle]) ->
    {ok, myer_handle:version(Handle), Handle}.


-spec ping(args()) -> {ok,result(),handle()}|{error,_,handle()}.
ping(Args) ->
    loop(Args, [fun ping_pre/1, fun send/2, fun recv/1]).

-spec refresh(args()) -> {ok,result(),handle()}|{error,_,handle()}.
refresh(Args) ->
    loop(Args, [fun refresh_pre/2, fun send/2, fun recv/1]).

-spec select_db(args()) -> {ok,result(),handle()}|{error,_,handle()}.
select_db(Args) ->
    loop(Args, [fun select_db_pre/2, fun send/2, fun recv/1]).


-spec real_query(args()) ->
                        {ok,result(),handle()}|
                        {ok,fields(),rows(),result(),handle()}|
                        {error,_,handle()}.
real_query(Args) ->
    loop(Args, [fun query_pre/2, fun send/2, fun query_post/1,
                fun recv_fields/2, fun recv_rows/3]).


-spec stmt_prepare(args()) -> {ok,prepare(),handle()}|{error,_,handle()}.
stmt_prepare(Args) ->
    loop(Args, [fun stmt_prepare_pre/2, fun send/2, fun stmt_prepare_post/1,
                fun stmt_prepare_recv_params/2, fun stmt_prepare_recv_fields/2]).

-spec stmt_close(args()) -> {ok,handle()}|{error,_,handle()}. % != result()
stmt_close(Args) ->
    loop(Args, [fun stmt_close_pre/2, fun send/2, fun stmt_close_post/1]).

-spec stmt_reset(args()) -> {ok,prepare(),handle()}|{error,_,handle()}.
stmt_reset(Args) ->
    loop(Args, [fun stmt_reset_pre/2, fun send/3, fun stmt_reset_post/2]).

-spec stmt_execute(args()) ->
                          {ok,prepare(),handle()}|
                          {ok,fields(),rows(),prepare(),handle()}|
                          {error,_,handle()}.
stmt_execute(Args) ->
    loop(Args, [fun stmt_execute_pre/3, fun send/3, fun stmt_execute_post/2,
                fun recv_stmt_fields/3, fun recv_stmt_rows/3]).

-spec stmt_fetch(args()) ->
                        {ok,prepare(),handle()}|
                        {ok,fields(),rows(),prepare(),handle()}|
                        {error,_,handle()}.
stmt_fetch(Args) ->
    loop(Args, [fun stmt_fetch_pre/2, fun send/3, fun recv_stmt_rows/2]).

%% -- internal: connect --

connect_pre(Address, Port, MaxLength, Compress, Timeout) ->
    case myer_handle:connect(Address, Port, MaxLength, timer:seconds(Timeout)) of
        {ok, Handle} ->
            {ok, [MaxLength,myer_handle:caps(Handle,default_caps(Compress))]};
        {error, Reason} ->
            {error, Reason}
    end.

connect_post(MaxLength, Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<10>>, Next} -> % "always 10"
            recv_handshake(MaxLength, Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next)
    end.

%% -- internal: close --

close_pre(Handle) ->
    {ok, [<<?COM_QUIT>>,Handle]}.

close_post(Handle) ->
    _ = myer_handle:close(Handle),
    {ok, [undefined]}.

%% -- internal: auth --

auth_pre(Handle, User, Password, <<>>, Charset, #handshake{caps=C}=H) ->
    Handshake = H#handshake{caps = (C bxor ?CLIENT_CONNECT_WITH_DB), charset = Charset},
    {ok, [auth_to_binary(User,Password,<<>>,Handshake),Handshake,Handle]};
auth_pre(Handle, User, Password, Database, Charset, #handshake{caps=C}=H) ->
    Handshake = H#handshake{caps = (C bxor ?CLIENT_NO_SCHEMA), charset = Charset},
    {ok, [auth_to_binary(User,Password,Database,Handshake),Handshake,Handle]}.

auth_post(#handshake{caps=C,version=V}=H, Handle) ->
    case recv_binary(1, myer_handle:version(myer_handle:caps(Handle,C),V)) of
        {ok, <<0>>, Next} ->
            recv_result(Next);
        {ok, <<254>>, Next} ->
            recv_plugin(H, Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next)
    end.


auth_alt_pre(Handle, Password, #handshake{seed=S,plugin=P}) ->
    Scrambled = myer_auth:scramble(Password, S, P),
    {ok, [<<Scrambled/binary,0>>,Handle]}.

auth_alt_post(Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<0>>, Next} ->
            recv_result(Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next)
    end.

%% -- internal: ping --

ping_pre(Handle) ->
    {ok, [<<?COM_PING>>,reset(Handle)]}.

%% -- internal: stat --

stat_pre(Handle) ->
    {ok, [<<?COM_STATISTICS>>,reset(Handle)]}.

stat_post(Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<255>>, Next} ->
            recv_reason(Next);
        {ok, Byte, Next} ->
            recv_remains(Byte, Next)
    end.

%% -- internal: refresh --

refresh_pre(Handle, Options) ->
    {ok, [<<?COM_REFRESH,Options/little>>,reset(Handle)]}.

%% -- internal: select_db --

select_db_pre(Handle, Database) ->
    {ok, [<<?COM_INIT_DB,Database/binary>>,reset(Handle)]}.

%% -- internal: query --

query_pre(Handle, Query) ->
    {ok, [<<?COM_QUERY,Query/binary>>,reset(Handle)]}.

query_post(Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<0>>, Next} ->
            recv_result(Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next);
        {ok, Byte, Next} ->
            recv_fields_length(Byte, Next)
    end.

%% -- internal: stmt_prepare --

stmt_prepare_pre(Handle, Query) ->
    {ok, [<<?COM_STMT_PREPARE,Query/binary>>,reset(Handle)]}.

stmt_prepare_post(Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<0>>, Next} ->
            recv_prepare(Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next)
    end.

stmt_prepare_recv_params(#prepare{param_count=0}=P, Handle) ->
    {ok, [P#prepare{params = [], result = undefined},Handle]};
stmt_prepare_recv_params(#prepare{param_count=N}=P, Handle) ->
    case recv_until_eof(recv_fields_func(Handle), [], [], Handle) of
        {ok, [Result,Params,Next]} when N == length(Params)->
            {ok, [P#prepare{params = Params, result = Result},Next]}
    end.

stmt_prepare_recv_fields(#prepare{field_count=0}=P, Handle) ->
    {ok, [P#prepare{fields = [], result = undefined},Handle]};
stmt_prepare_recv_fields(#prepare{field_count=N}=P, Handle) ->
    case recv_until_eof(recv_fields_func(Handle), [], [], Handle) of
        {ok, [Result,Fields,Next]} when N == length(Fields) ->
            {ok, [P#prepare{fields = Fields, result = Result},Next]}
    end.

%% -- internal: stmt_close --

stmt_close_pre(Handle, #prepare{stmt_id=S}) ->
    {ok, [<<?COM_STMT_CLOSE,S:32/little>>,reset(Handle)]}.

stmt_close_post(Handle) ->
    {ok, [Handle]}.

%% -- internal: stmt_reset --

stmt_reset_pre(Handle, #prepare{stmt_id=S}=P) ->
    {ok, [<<?COM_STMT_RESET,S:32/little>>,P,reset(Handle)]}.

stmt_reset_post(Prepare, Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<0>>, Next} ->
            recv_stmt_reset(Prepare, Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next)
    end.

%% -- internal: stmt_execute --

stmt_execute_pre(Handle, Prepare, Params) ->
    {ok, [stmt_execute_to_binary(Prepare,Params),Prepare,reset(Handle)]}.

stmt_execute_post(Prepare, Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<0>>, Next} ->
            recv_stmt_result(Prepare, Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next);
        {ok, Byte, Next} ->
            recv_fields_length(Byte, Prepare, Next)
    end.

%% -- internal: stmt_fetch --

stmt_fetch_pre(#prepare{stmt_id=S,prefetch_rows=R}=P, Handle) ->
    {ok, [<<?COM_STMT_FETCH,S:32/little,R:32/little>>,P,reset(Handle)]}.

%% -spec next_result(protocol())
%%                  -> {ok,result(),protocol()}|
%%                     {ok,{[field()],[term()],result()},protocol()}|{error,_,protocol()}.
%% next_result(#protocol{handle=H}=P)
%%   when undefined =/= H ->
%%     loop([P], [fun next_result_pre/1, fun recv_status/1, fun query_post/3]).

%% -spec stmt_next_result(protocol(),prepare())
%%                       -> {ok,prepare()}|{ok,[field()],[term()],prepare(),protocol()}|{error,_, protocol()}.
%% stmt_next_result(#protocol{handle=H}=P, #prepare{}=X)
%%   when undefined =/= H ->
%%     loop([X,P], [fun stmt_next_result_pre/2, fun recv_status/2, fun stmt_next_result_post/3]).

%% -- internal: loop,next_result --

%% next_result_pre(#protocol{}=P) ->
%%     {ok, [reset(P)]}.

%% -- internal: loop,stmt_next_result --

%% stmt_next_result_pre(#prepare{}=X, #protocol{}=P) ->
%%     {ok, [X,reset(P)]}.

%% stmt_next_result_post(<<0>>, #prepare{execute=E}=X, #protocol{}=P) ->
%%     case recv_result(P) of
%%         {ok, [Result,Protocol]} ->
%%             {ok, [X#prepare{result = Result, execute = E+1},Protocol]};
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end.

%% == internal ==

binary_to_float(Binary, _Decimals) ->
    try binary_to_float(Binary)
    catch
        _:_ ->
            try binary_to_integer(Binary) of
                I ->
                    I * 1.0
            catch
                _:_ ->
                    undefined % for v5.1 (out_of_range)
            end
    end.

binary_to_version(Binary) ->
    F = fun(E) -> try binary_to_integer(E,10) catch _:_ -> E end end,
    L = binary:split(<<Binary/binary,".0.0">>, [<<$.>>,<<$->>], [global]),
    lists:map(F, lists:sublist(L,3)).

loop(Args, []) ->
    list_to_tuple([ok|Args]);
loop(Args, [H|T]) ->
    try apply(H, Args) of
        {ok, List} ->
            loop(List, T);
        {error, Reason} ->         % connect_pre/4, send/2-3, recv_reason/3
            {error, Reason}
    catch
        {error, Reason, Handle} -> % recv_binary/2, recv_text/2, send/2-3
            {error, Reason, Handle}
    end.

recv(Handle) -> % < loop
    case recv_binary(1, Handle) of
        {ok, <<0>>, Next} ->
            recv_result(Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next)
    end.

recv_fields_func(_) -> % TODO
    fun myer_protocol_text_old:recv_field_41/2.

recv_fields_length(Byte, Handle) ->
    case recv_packed_unsigned(Byte, Handle) of
        {ok, U, Next} ->
            {ok, [U,Next]}
    end.

recv_fields_length(Byte, Term, Handle) ->
    case recv_packed_unsigned(Byte, Handle) of
        {ok, U, Next} ->
            {ok, [U,Term,Next]}
    end.

recv_fields(_, Handle) -> % < loop
    recv_until_eof(recv_fields_func(Handle), [], [], Handle).

recv_packed_binary(Handle) ->
    case recv_packed_unsigned(Handle) of
        {ok, null, Next} ->
            {ok, null, Next};
        {ok, 0, Next} ->
            {ok, <<>>, Next};
        {ok, Length, Next} ->
            recv_binary(Length, Next)
    end.

recv_packed_binary(Byte, Handle) ->
    case recv_packed_unsigned(Byte, Handle) of
        {ok, null, Next} ->
            {ok, null, Next};
        {ok, 0, Next} ->
            {ok, <<>>, Next};
        {ok, Length, Next} ->
            recv_binary(Length, Next)
    end.

recv_packed_unsigned(Handle) ->
    case recv_binary(1, Handle) of
        {ok, Byte, Next} ->
            recv_packed_unsigned(Byte, Next)
    end.

recv_remains(Byte, Handle) ->
    case recv_binary(remains(Handle), Handle) of
        {ok, Binary, Next} ->
            {ok, [<<Byte/binary,Binary/binary>>,Next]}
    end.

recv_rows(_Result, Fields, Handle) -> % < loop
    case recv_until_eof(fun myer_protocol_text:recv_row/3, [Fields], [], Handle) of
        {ok, [Result,Rows,Next]} ->
            {ok, [Fields,Rows,Result,Next]}
    end.

recv_stmt_fields(0, Prepare, Handle) -> % < loop
    {ok, Prepare, Handle};
recv_stmt_fields(U, #prepare{field_count=U,fields=L}=P, Handle) ->
    case recv_until_eof(recv_fields_func(Handle), [], [], Handle) of
        {ok, [#result{status=S}=R,L,Next]} ->
            F = myer_protocol_binary:prepare_fields(L), % force
            {ok, [S,P#prepare{fields = F, result = R},Next]}
    end.

recv_stmt_reset(#prepare{}=P, Handle) ->
    case recv_result(Handle) of
        {ok, [Result, Next]} ->
            {ok, [P#prepare{result = Result, execute = 0},Next]}
    end.

recv_stmt_result(#prepare{execute=E}=P, Handle) ->
    case recv_result(Handle) of
        {ok, [Result,Next]} ->
            {ok, [0,P#prepare{result = Result, execute = E+1},Next]}
    end.

recv_stmt_rows(#prepare{result=R}=P, Handle) ->
    recv_stmt_rows(R#result.status, P, Handle).

recv_stmt_rows(Status, Prepare, Handle)
  when ?IS_SET(Status,?SERVER_STATUS_CURSOR_EXISTS) ->
    {ok, [Prepare,Handle]};
recv_stmt_rows(_Status, #prepare{fields=F}=P, Handle) ->
    case recv_until_eof(fun myer_protocol_binary:recv_row/3, [F], [], Handle) of
        {ok, [Result,Rows,Next]} ->
            {ok, [Rows,P#prepare{result = Result},Next]}
    end.

recv_unsigned(Length, Handle) ->
    case recv_binary(Length, Handle) of
        {ok, Binary, Next} ->
            {ok, binary:decode_unsigned(Binary,little), Next}
    end.

recv_until_eof(Func, Args, List, Handle) ->
    case recv_binary(1, Handle) of
        {ok, <<254>>, Next} ->
            recv_eof(lists:reverse(List), Next);
        {ok, <<255>>, Next} ->
            recv_reason(Next);
        {ok, Byte, Next} ->
            recv_until_eof(Func, Args, List, Byte, Next)
    end.

recv_until_eof(Func, Args, List, Byte, Handle) ->
    case apply(Func, [Handle|[Byte|Args]]) of
        {ok, Term, Next} ->
            recv_until_eof(Func, Args, [Term|List], Next)
    end.

send(Binary, Handle) ->
    case myer_handle:send(Binary, Handle) of
        {ok, Next} ->
            {ok, [Next]}
    end.

send(Binary, Term, Handle) ->
    case myer_handle:send(Binary, Handle) of
        {ok, Next} ->
            {ok, [Term,Next]}
    end.

%% -----------------------------------------------------------------------------
%% << include/mysql_com.h : CLIENT_ALL_FLAGS
%% -----------------------------------------------------------------------------
default_caps(false) ->
    default_caps(0);
default_caps(true) ->
    default_caps(?CLIENT_COMPRESS);
default_caps(Caps) ->
    L = [
         ?CLIENT_LONG_PASSWORD,
         ?CLIENT_FOUND_ROWS,
         ?CLIENT_LONG_FLAG,
         ?CLIENT_CONNECT_WITH_DB,
         ?CLIENT_NO_SCHEMA,
         %%?CLIENT_LOCAL_FILES
         %%?CLIENT_IGNORE_SPACE
         ?CLIENT_PROTOCOL_41,
         %%?CLIENT_INTERACTIVE
         %%?CLIENT_IGNORE_SIGPIPE
         ?CLIENT_TRANSACTIONS,
         ?CLIENT_SECURE_CONNECTION, % > 5.7, CLIENT_RESERVED2
         ?CLIENT_MULTI_STATEMENTS,
         ?CLIENT_MULTI_RESULTS,
         ?CLIENT_PS_MULTI_RESULTS
         %%?CLIENT_REMEMBER_OPTIONS
         %%?CLIENT_PLUGIN_AUTH                         % TODO
         %%?CLIENT_CONNECT_ATTRS
         %%?CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA
         %%?CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS
         %%?CLIENT_SESSION_TRACK
         %%?CLIENT_DEPRECATE_EOF
        ],
    lists:foldl(fun(E,A) -> A bor E end, Caps, L).

%% -----------------------------------------------------------------------------
%% << sql-common/pack.c : *
%% -----------------------------------------------------------------------------

pack_binary(Binary) ->
    pack_binary(Binary, byte_size(Binary)).

pack_binary(_Binary, 0) ->
    <<251>>;
pack_binary(Binary, Length) ->
    B = pack_integer(Length),
    <<B/binary,Binary/binary>>.

pack_integer(Value) when Value < 251      -> <<Value>>;
pack_integer(Value) when Value < 65536    -> <<252,Value:16/little>>;
pack_integer(Value) when Value < 16777216 -> <<253,Value:24/little>>;
pack_integer(Value)                       -> <<254,Value:64/little>>.

recv_packed_unsigned(<<254>>, Handle) -> recv_unsigned(8, Handle);
recv_packed_unsigned(<<253>>, Handle) -> recv_unsigned(3, Handle);
recv_packed_unsigned(<<252>>, Handle) -> recv_unsigned(2, Handle);
recv_packed_unsigned(<<251>>, Handle) -> {ok, null, Handle};
recv_packed_unsigned(<<Int>>, Handle) -> {ok, Int, Handle}.

%% -----------------------------------------------------------------------------
%% << sql-common/client.c : send_client_reply_packet/3
%% -----------------------------------------------------------------------------
auth_to_binary(User, Password, Database,
               #handshake{maxlength=M,seed=S,caps=C,charset=E,plugin=P})
  when ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    X = myer_auth:scramble(Password, S, P),
    B = if ?IS_SET(C,?CLIENT_SECURE_CONNECTION) -> N = size(X), <<N,X/binary>>;
           true                                -> <<X/binary,0>>
        end,
    D = if ?IS_SET(C,?CLIENT_CONNECT_WITH_DB) -> <<Database/binary,0>>;
           true                              -> <<0>>
        end,
    <<
      C:32/little,
      M:32/little,
      E,
      0:23/integer-unit:8,
      User/binary, 0,
      B/binary,
      D/binary,
      P/binary
    >>;
auth_to_binary(User, Password, Database,
               #handshake{maxlength=M,seed=S,caps=C,plugin=P}) ->
    A = (C bor ?CLIENT_LONG_PASSWORD) band 16#ffff, % FORCE
    X = myer_auth:scramble(Password, S, P),
    B = if ?IS_SET(C,?CLIENT_SECURE_CONNECTION) -> N = size(X), <<N,X/binary>>;
           true                                -> <<X/binary,0>>
        end,
    D = if ?IS_SET(C,?CLIENT_CONNECT_WITH_DB) -> <<Database/binary,0>>;
           true                              -> <<>>
        end,
    <<
      A:16/little,
      M:24/little,
      User/binary, 0,
      B/binary,
      D/binary
    >>.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_eof/3
%%    sql/protocol_classic.cc : net_send_eof/3
%% -----------------------------------------------------------------------------
recv_eof(Term, Handle) ->
    recv_eof(#result{}, Term, myer_handle:caps(Handle), Handle).

recv_eof(#result{warning_count=undefined}=R, Term, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_eof(R#result{warning_count = U}, Term, Caps, H);
recv_eof(#result{warning_count=undefined}=R, Term, Caps, Handle) ->
    recv_eof(R#result{warning_count = 0}, Term, Caps, Handle);
recv_eof(#result{status=undefined}=R, Term, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_eof(R#result{status = U}, Term, Caps, H);
recv_eof(#result{status=undefined}=R, Term, Caps, Handle) ->
    recv_eof(R#result{status = 0}, Term, Caps, Handle);
recv_eof(#result{}=R, Term, _Caps, Handle) ->
    {ok, [R,Term,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_server_handshake_packet/3
%%    sql/auth/sql_authentication.cc : send_server_handshake_packet/3
%% -----------------------------------------------------------------------------
recv_handshake(MaxLength, Handle) ->
    recv_handshake(#handshake{
                      maxlength = MaxLength
                     }, myer_handle:caps(Handle), Handle).

recv_handshake(#handshake{version=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{version = binary_to_version(B)}, Caps, H);
recv_handshake(#handshake{tid=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(4, Handle),
    recv_handshake(R#handshake{tid = U}, Caps, H);
recv_handshake(#handshake{seed1=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{seed1 = B}, Caps, H);
recv_handshake(#handshake{caps1=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_handshake(R#handshake{caps1 = U}, Caps, H);
recv_handshake(#handshake{charset=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(1, Handle),
    recv_handshake(R#handshake{charset = U}, Caps, H);
recv_handshake(#handshake{status=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_handshake(R#handshake{status = U}, Caps, H);
recv_handshake(#handshake{version=V,caps1=C,caps2=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_handshake(R#handshake{caps2 = U}, Caps, H);
recv_handshake(#handshake{caps2=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{caps2 = 0}, Caps, Handle);
recv_handshake(#handshake{version=V,caps1=C,length=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(1, Handle),
    recv_handshake(R#handshake{length = U}, Caps, H);
recv_handshake(#handshake{length=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{length = 8}, Caps, Handle);   % 8?, TODO
recv_handshake(#handshake{version=V,caps1=C,reserved=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_binary(10, Handle),
    recv_handshake(R#handshake{reserved = B}, Caps, H); % "always 0"
recv_handshake(#handshake{reserved=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_binary(13, Handle),
    recv_handshake(R#handshake{reserved = B}, Caps, H); % "always 0"?
recv_handshake(#handshake{version=V,caps1=C,seed2=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{seed2 = B}, Caps, H);
recv_handshake(#handshake{seed2=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{seed2 = <<>>}, Caps, Handle);
recv_handshake(#handshake{version=V,caps1=C,plugin=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{plugin = B}, Caps, H);
recv_handshake(#handshake{plugin=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{plugin = <<"mysql_native_password">>}, Caps, Handle);
recv_handshake(#handshake{caps1=C1,caps2=C2,seed1=S1,seed2=S2}=R, Caps, Handle) ->
    {ok, [R#handshake{caps = Caps band ((C2 bsl 16) bor C1), seed = <<S1/binary,S2/binary>>}, Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_plugin_request_packet/3
%%    sql/auth/sql_authentication.cc : send_plugin_request_packet/3
%% -----------------------------------------------------------------------------
recv_plugin(Term, Handle) ->
    recv_plugin(#plugin{}, Term, myer_handle:caps(Handle), Handle).

recv_plugin(#plugin{name=undefined}=P, Handshake, Caps, Handle) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_plugin(P#plugin{name = B}, Handshake, Caps, H);
recv_plugin(#plugin{}=P, Handshake, _Caps, Handle) ->
    {ok, [P,Handshake,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_prepare.cc : send_prep_stmt/2
%% -----------------------------------------------------------------------------
recv_prepare(Handle) ->
    recv_prepare(#prepare{
                    flags = ?CURSOR_TYPE_NO_CURSOR,
                    prefetch_rows = 1,
                    execute = 0
                   },
                 myer_handle:caps(Handle), Handle).

recv_prepare(#prepare{stmt_id=undefined}=P, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(4, Handle),
    recv_prepare(P#prepare{stmt_id=U}, Caps, H);
recv_prepare(#prepare{field_count=undefined}=P, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_prepare(P#prepare{field_count=U}, Caps, H);
recv_prepare(#prepare{param_count=undefined}=P, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_prepare(P#prepare{param_count=U}, Caps, H);
recv_prepare(#prepare{reserved=undefined}=P, Caps, Handle) ->
    {ok, B, H} = recv_binary(1, Handle), % <<0>>
    recv_prepare(P#prepare{reserved=B}, Caps, H);
recv_prepare(#prepare{warning_count=undefined}=P, Caps, Handle) -> % < 5.0, undefined?
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_prepare(P#prepare{warning_count=U}, Caps, H);
recv_prepare(#prepare{}=P, _Caps, Handle) ->
    {ok, [P,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_error_packet/4
%%    sql/protocol_classic.cc : net_send_error_packet/7
%% -----------------------------------------------------------------------------
recv_reason(Handle) ->
    recv_reason(#reason{}, myer_handle:caps(Handle), Handle).

recv_reason(#reason{errno=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_reason(R#reason{errno = U}, Caps, H);
recv_reason(#reason{reserved=undefined}=R, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_binary(1, Handle), % <<$#>>
    recv_reason(R#reason{reserved = B}, Caps, H);
recv_reason(#reason{state=undefined}=R, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_binary(5, Handle),
    recv_reason(R#reason{state = B}, Caps, H);
recv_reason(#reason{message=undefined}=R, Caps, Handle)->
    {ok, B, H} = recv_binary(remains(Handle), Handle),
    recv_reason(R#reason{message = B}, Caps, H);
recv_reason(#reason{}=R, _Caps, Handle) ->
    {error, R, Handle}. % != ok

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_ok/6
%%    sql/protocol_classic.cc : net_send_ok/7
%% -----------------------------------------------------------------------------
recv_result(Handle) ->
    recv_result(#result{}, myer_handle:caps(Handle), Handle).

recv_result(#result{affected_rows=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_packed_unsigned(Handle),
    recv_result(R#result{affected_rows = U}, Caps, H);
recv_result(#result{insert_id=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_packed_unsigned(Handle),
    recv_result(R#result{insert_id = U}, Caps, H);
recv_result(#result{status=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_result(R#result{status = U}, Caps, H);
recv_result(#result{warning_count=undefined}=R, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_result(R#result{warning_count = U}, Caps, H);
recv_result(#result{message=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_binary(remains(Handle), Handle),
    recv_result(R#result{message = B}, Caps, H);
recv_result(#result{}=R, _Caps, Handle) ->
    {ok, [R,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_prepare.cc : *, TODO,TODO,TODO
%% -----------------------------------------------------------------------------
stmt_execute_fold_args([], _, _, L1, L2, B) ->
    {lists:reverse(L1), lists:reverse(L2), B};
stmt_execute_fold_args([H|T], N, P, L1, L2, B)
  when is_integer(H), 0 > H ->
    B1 = <<?MYSQL_TYPE_LONGLONG:16/little>>,
    B2 = <<H:8/integer-unsigned-little-unit:8>>,
    stmt_execute_fold_args(T, N, P+1, [B1|L1], [B2|L2], B);
stmt_execute_fold_args([H|T], N, P, L1, L2, B)
  when is_integer(H) ->
    B1 = <<(?MYSQL_TYPE_LONGLONG bor (1 bsl 15)):16/little>>,
    B2 = <<H:8/integer-unsigned-little-unit:8>>,
    stmt_execute_fold_args(T, N, P+1, [B1|L1], [B2|L2], B);
stmt_execute_fold_args([H|T], N, P, L1, L2, B)
  when is_float(H), 0 > H ->
    B1 = <<?MYSQL_TYPE_DOUBLE:16/little>>,
    B2 = <<H:8/float-unsigned-little-unit:8>>,
    stmt_execute_fold_args(T, N, P+1, [B1|L1], [B2|L2], B);
stmt_execute_fold_args([H|T], N, P, L1, L2, B)
  when is_float(H) ->
    B1 = <<(?MYSQL_TYPE_DOUBLE bor (1 bsl 15)):16/little>>,
    B2 = <<H:8/float-unsigned-little-unit:8>>,
    stmt_execute_fold_args(T, N, P+1, [B1|L1], [B2|L2], B);
stmt_execute_fold_args([H|T], N, P, L1, L2, B)
  when is_binary(H), 65536 > size(H) ->
    B1 = <<?MYSQL_TYPE_VAR_STRING:16/little>>,
    B2 = pack_binary(H),
    stmt_execute_fold_args(T, N, P+1, [B1|L1], [B2|L2], B);
stmt_execute_fold_args([H|T], N, P, L1, L2, B)
  when is_binary(H) -> % 16777186 < size(H) -> 'incorrect arguments to mysqld_stmt_execute'??
    B1 = <<?MYSQL_TYPE_BLOB:16/little>>,
    B2 = pack_binary(H),
    stmt_execute_fold_args(T, N, P+1, [B1|L1], [B2|L2], B);
stmt_execute_fold_args([null|T], N, P, L1, L2, B) ->
    B1 = <<0:16/little>>, % dummy
    X = 1 bsl ((P band 7) + 8 * (N - P div 8 - 1)),
    stmt_execute_fold_args(T, N, P+1, [B1|L1], L2, B bor X).

stmt_execute_to_binary(#prepare{stmt_id=S,flags=F,execute=_E}, Params) ->
    W = (length(Params) + 7) div 8,
    {T, D, U} = stmt_execute_fold_args(Params, W, 0, [], [], 0),
    B = if %0 < E -> iolist_to_binary(lists:flatten([<<0>>,D]));
            true  -> iolist_to_binary(lists:flatten([<<1>>,T,D]))
        end,
    <<
      ?COM_STMT_EXECUTE,
      S:32/little,         % stmt_id
      F,                   % flags=0 : CURSOR_TYPE_NO_CURSOR (def)
      %%                   %       1 : CURSOR_TYPE_READ_ONLY -> open_cursor
      %%                   %            -> SERVER_STATUS_CURSOR_EXISTS (-> fetch)
      0,0,0,0,             % ?
      U:W/unsigned-unit:8, % null_bits
      B/binary             % types_suppled_by_client,([type],)[data]
    >>.
