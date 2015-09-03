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

%% -- public --
%% -export([stmt_close/2, stmt_execute/3, stmt_fetch/2, stmt_reset/2]).
%% -export([next_result/1, stmt_next_result/2]).

%% -- private --
-export([connect/1, close/1, auth/1, version/1]).
-export([ping/1, stat/1, refresh/1, select_db/1]).
-export([query/1]).
-export([stmt_prepare/1]).

-export([binary_to_float/2,
         recv/3, recv_packed_binary/3, recv_unsigned/3]).

%% == private ==

-spec connect([term()]) -> {ok,handshake(),handle()}|{error,_}|{error,_,handle()}.
connect(Args) ->
    loop(Args, [fun connect_pre/4, fun connect_post/2]).

-spec close([term()]) -> ok|{error,_,handle()}.
close(Args) ->
    loop(Args, [fun close_pre/1, fun send/2, fun close_post/1]).

-spec auth([term()]) -> {ok,result(),handle()}|{error,_,handle()}.
auth(Args) ->
    case loop(Args, [fun auth_pre/6, fun send/3, fun auth_post/2]) of
        {ok, #plugin{name=N}, #handshake{}=H, Handle} ->
            auth_alt([Handle,lists:nth(3,Args),H#handshake{plugin = N}]);
        {ok, #result{}=R, Handle}->
            {ok, R, Handle};
        {error, Reason, Handle} ->
            {error, Reason, Handle}
    end.

auth_alt(Args) ->
    loop(Args, [fun auth_alt_pre/3, fun send/3, fun auth_alt_post/1]).

-spec version(handshake()) -> version().
version(#handshake{version=V}) ->
    V.


-spec ping([term()]) -> {ok,result(),handle()}|{error,_,handle()}.
ping(Args) ->
    loop(Args, [fun ping_pre/1, fun send/2, fun recv_result/1]).

-spec stat([term()]) -> {ok,binary(),handle()}|{error,_,handle()}.
stat(Args) ->
    loop(Args, [fun stat_pre/1, fun send/2, fun stat_post/1]).

-spec refresh([term()]) -> {ok,result(),handle()}|{error,_,handle()}.
refresh(Args) ->
    loop(Args, [fun refresh_pre/2, fun send/2, fun recv_result/1]).

-spec select_db([term()]) -> {ok,result(),handle()}|{error,_,handle()}.
select_db(Args) ->
    loop(Args, [fun select_db_pre/2, fun send/2, fun recv_result/1]).


-spec query([term()])
           -> {ok,result(),handle()}|
              {ok,fields(),rows(),result(),handle()}|
              {error,_,handle()}.
query(Args) ->
    loop(Args, [fun query_pre/2, fun send/2, fun query_post/1,
                fun recv_fields/2, fun recv_rows/2]).


-spec stmt_prepare([term()]) -> {ok,prepare(),handle()}|{error,_,handle()}.
stmt_prepare(Args) ->
    loop(Args, [fun stmt_prepare_pre/2, fun send/2, fun stmt_prepare_post/1,
                fun stmt_prepare_recv_params/2, fun stmt_prepare_recv_fields/2]).

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
        {error, Reason} ->                      % connect_pre/4, recv_reason/3
            {error, Reason}
    catch
        {error, Reason, Handle} ->              % recv/3
            {error, Reason, Handle}
    end.

recv(Term, Caps, #handle{}=S) ->
    case myer_handle:recv(S, Term, ?IS_SET(Caps,?CLIENT_COMPRESS)) of
        {ok, Binary, Handle} ->
            {ok, Binary, Handle};
        {error, Reason} ->
            throw({error,Reason,S})
    end.

%%recv_fields_func(Caps) % TODO
%%  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
%%    fun myer_protocol_text:recv_field_41/3;
recv_fields_func(_Caps) ->
    fun myer_protocol_text:recv_field/3.

recv_fields_length(Byte, Caps, #handle{}=S) ->
    case recv_packed_unsigned(Byte, Caps, S) of
        {ok, U, Handle} ->
            {ok, [U,Handle]}
    end.

recv_fields(U, #handle{caps=C}=S) -> % < loop
    case recv_until_eof(recv_fields_func(S), [], [], C, S) of
        {ok, [_Result,Fields,Handle]} when U == length(Fields) ->
            {ok, [Fields,Handle]}
    end.

recv_packed_binary(Byte, Caps, #handle{}=S) ->
    case recv_packed_unsigned(Byte, Caps, S) of
        {ok, null, Handle} ->
            {ok, null, Handle};
        {ok, 0, Handle} ->
            {ok, <<>>, Handle};
        {ok, Length, Handle} ->
            recv(Length, Caps, Handle)
    end.

recv_packed_unsigned(Caps, #handle{}=S) ->
    case recv(1, Caps, S) of
        {ok, Byte, Handle} ->
            recv_packed_unsigned(Byte, Caps, Handle)
    end.

recv_packed_unsigned(undefined, Caps, Handle) -> recv_packed_unsigned(Caps, Handle);
recv_packed_unsigned(<<254>>,   Caps, Handle) -> recv_unsigned(8, Caps, Handle);
recv_packed_unsigned(<<253>>,   Caps, Handle) -> recv_unsigned(3, Caps, Handle);
recv_packed_unsigned(<<252>>,   Caps, Handle) -> recv_unsigned(2, Caps, Handle);
recv_packed_unsigned(<<251>>,  _Caps, Handle) -> {ok, null, Handle};
recv_packed_unsigned(<<Int>>,  _Caps, Handle) -> {ok, Int, Handle}.

recv_remains(Byte, Caps, #handle{}=S) ->
    case recv(remains(S), Caps, S) of
        {ok, Binary, Handle} ->
            {ok, [<<Byte/binary,Binary/binary>>,Handle]}
    end.

recv_result(#handle{caps=C}=S) -> % < loop
    case recv(1, C, S) of
        {ok, <<0>>, Handle} ->
            recv_result(#result{}, C, Handle);
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle)
    end.

recv_rows(Fields, #handle{caps=C}=S) -> % < loop
    case recv_until_eof(fun myer_protocol_text:recv_row/4, [Fields], [], C, S) of
        {ok, [Result,Rows,Handle]} ->
            {ok, [Fields,Rows,Result,Handle]}
    end.

recv_unsigned(Length, Caps, #handle{}=S) ->
    case recv(Length, Caps, S) of
        {ok, Binary, Handle} ->
            {ok, binary:decode_unsigned(Binary,little), Handle}
    end.

recv_until_eof(Func, Args, List, Caps, #handle{}=S) ->
    case recv(1, Caps, S) of
        {ok, <<254>>, Handle} ->
            recv_eof(#result{}, lists:reverse(List), Caps, Handle);
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, Caps, Handle);
        {ok, Byte, Handle} ->
            recv_until_eof(Func, Args, List, Byte, Caps, Handle)
    end.

recv_until_eof(Func, Args, List, Byte, Caps, #handle{}=S) ->
    case apply(Func, [S|[Caps|[Byte|Args]]]) of
        {ok, Term, Handle} ->
            recv_until_eof(Func, Args, [Term|List], Caps, Handle)
    end.

remains(Handle) ->
    myer_handle:remains(Handle).

reset(Handle) ->
    myer_handle:reset(Handle).

send(Binary, #handle{caps=C}=S) ->
    case myer_handle:send(S, Binary, ?IS_SET(C,?CLIENT_COMPRESS)) of
        {ok, Handle} ->
            {ok, [Handle]};
        {error, Reason} ->
            {error, Reason}
    end.

send(Term, Binary, #handle{caps=C}=S) ->
    case myer_handle:send(S, Binary, ?IS_SET(C,?CLIENT_COMPRESS)) of
        {ok, Handle} ->
            {ok, [Term,Handle]};
        {error, Reason} ->
            {error, Reason}
    end.

%% -- internal: connect --

connect_pre(Address, Port, MaxLength, Timeout) ->
    case myer_handle:connect(Address, Port, MaxLength, timer:seconds(Timeout)) of
        {ok, #handle{}=H} ->
            {ok, [MaxLength,H#handle{caps = default_caps(false)}]};
        {error, Reason} ->
            {error, Reason}
    end.

connect_post(MaxLength, #handle{caps=C}=H) ->
    case recv(1, C, H) of
        {ok, <<10>>, Handle} -> % "always 10"
            recv_handshake(#handshake{maxlength = MaxLength}, C, Handle);
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle)
    end.

%% -- internal: close --

close_pre(#handle{}=H) ->
    {ok, [<<?COM_QUIT>>,H]}.

close_post(#handle{}=H) ->
    _ = myer_handle:close(H),
    {ok, [undefined]}.

%% -- internal: auth --

auth_pre(#handle{}=H, User, Password, <<>>, Charset, #handshake{caps=C}=R) ->
    Handshake = R#handshake{caps = (C bxor ?CLIENT_CONNECT_WITH_DB), charset = Charset},
    {ok, [Handshake,auth_to_binary(User,Password,<<>>,Handshake),H]};
auth_pre(#handle{}=H, User, Password, Database, Charset, #handshake{caps=C}=R) ->
    Handshake = R#handshake{caps = (C bxor ?CLIENT_NO_SCHEMA), charset = Charset},
    {ok, [Handshake,auth_to_binary(User,Password,Database,Handshake),H]}.

auth_post(#handshake{caps=C,version=V}=R, #handle{}=H) ->
    case recv(1, C, H#handle{caps = C, version = V}) of
        {ok, <<0>>, Handle} ->
            recv_result(#result{}, C, Handle);
        {ok, <<254>>, Handle} ->
            recv_plugin(#plugin{}, R, C, Handle);
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle)
    end.


auth_alt_pre(#handle{}=H, Password, #handshake{seed=S,plugin=P}) ->
    Scrambled = myer_auth:scramble(Password, S, P),
    {ok, [<<Scrambled/binary,0>>,H]}.

auth_alt_post(#handle{caps=C}=H) ->
    case recv(1, C, H) of
        {ok, <<0>>, Handle} ->
            recv_result(#result{}, C, Handle);
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle)
    end.

%% -- internal: ping --

ping_pre(#handle{}=H) ->
    {ok, [<<?COM_PING>>,reset(H)]}.

%% -- internal: stat --

stat_pre(#handle{}=H) ->
    {ok, [<<?COM_STATISTICS>>,reset(H)]}.

stat_post(#handle{caps=C}=H) ->
    case recv(1, C, H) of
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle);
        {ok, Byte, Handle} ->
            recv_remains(Byte, C, Handle)
    end.

%% -- internal: refresh --

refresh_pre(#handle{}=H, Options) ->
    {ok, [<<?COM_REFRESH,Options/little>>,reset(H)]}.

%% -- internal: select_db --

select_db_pre(#handle{}=H, Database) ->
    {ok, [<<?COM_INIT_DB,Database/binary>>,reset(H)]}.

%% -- internal: query --

query_pre(#handle{}=H, Query) ->
    {ok, [<<?COM_QUERY,Query/binary>>,reset(H)]}.

query_post(#handle{caps=C}=H) ->
    case recv(1, C, H) of
        {ok, <<0>>, Handle} ->
            recv_result(#result{}, C, Handle);
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle);
        {ok, Byte, Handle} ->
            recv_fields_length(Byte, C, Handle)
    end.

%% -- internal: stmt_prepare --

stmt_prepare_pre(#handle{}=H, Query) ->
    {ok, [<<?COM_STMT_PREPARE,Query/binary>>,reset(H)]}.

stmt_prepare_post(#handle{caps=C}=H) ->
    case recv(1, C, H) of
        {ok, <<0>>, Handle} ->
            recv_prepare(#prepare{flags = ?CURSOR_TYPE_NO_CURSOR,
                                  prefetch_rows = 1}, C, Handle); % TODO
        {ok, <<255>>, Handle} ->
            recv_reason(#reason{}, C, Handle)
    end.

stmt_prepare_recv_params(#prepare{param_count=0}=P, #handle{}=H) ->
    {ok, [P#prepare{params = [], result = undefined},H]};
stmt_prepare_recv_params(#prepare{param_count=N}=P, #handle{caps=C}=H) ->
    case recv_until_eof(recv_fields_func(C), [], [], C, H) of
        {ok, [Result,Params,Handle]} when N == length(Params)->
            {ok, [P#prepare{params = Params, result = Result},Handle]};
        {error, Reason, Handle} ->
            {error, Reason, Handle}
    end.

stmt_prepare_recv_fields(#prepare{field_count=0}=P, #handle{}=H) ->
    {ok, [P,H]};
stmt_prepare_recv_fields(#prepare{field_count=N}=P, #handle{caps=C}=H) ->
    case recv_until_eof(recv_fields_func(C), [], [], C, H) of
        {ok, [Result,Fields,Handle]} when N == length(Fields) ->
            {ok, [P#prepare{fields = Fields, result = Result},Handle]};
        {error, Reason, Handle} ->
            {error, Reason, Handle}
    end.



%%  TODO,TODO

%% == public ==

%% -spec stmt_close(protocol(),prepare()) -> {ok,undefined,protocol()}|{error,_,protocol()}.
%% stmt_close(#protocol{handle=H}=P, #prepare{}=X)
%%   when undefined =/= H ->
%%     loop([X,P], [fun stmt_close_pre/2, fun send/3, fun stmt_close_post/2]).

%% -spec stmt_execute(protocol(),prepare(),[term()])
%%                   -> {ok,prepare(),protocol()}|
%%                      {ok,{[field()],[term()],prepare()},protocol()}|{error,_,protocol()}.
%% stmt_execute(#protocol{handle=H}=P, #prepare{param_count=N}=X, Args)
%%   when undefined =/= H, is_list(Args), N == length(Args) ->
%%     loop([X,Args,P], [fun stmt_execute_pre/3, fun send/3, fun recv_status/2, fun stmt_execute_post/3]).

%% -spec stmt_fetch(protocol(),prepare())
%%                 -> {ok,prepare(),protocol()}|
%%                    {ok,{[field()],[term()],prepare()},protocol()}|{error,_,protocol()}.
%% stmt_fetch(#protocol{handle=H}=P, #prepare{result=R}=X)
%%   when undefined =/= H ->
%%     case R#result.status of
%%         S when ?IS_SET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
%%             loop([X,P], [fun stmt_fetch_pre/2, fun send/3, fun stmt_fetch_post/2]);
%%         _ ->
%%             {ok, X, P}
%%     end.

%% -spec stmt_reset(protocol(),prepare()) -> {ok,prepare(),protocol()}|{error,_,protocol()}.
%% stmt_reset(#protocol{handle=H}=P, #prepare{}=X)
%%   when undefined =/= H ->
%%     loop([X,P], [fun stmt_reset_pre/2, fun send/3, fun recv_status/2, fun stmt_reset_post/3]).

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


%% == private ==

%% -- internal: loop,next_result --

%% next_result_pre(#protocol{}=P) ->
%%     {ok, [reset(P)]}.

%% -- internal: loop,stmt_close --

%% stmt_close_pre(#prepare{stmt_id=S}=X, #protocol{}=P) ->
%%     {ok, [X,<<?COM_STMT_CLOSE,S:32/little>>,reset(P)]}.

%% stmt_close_post(_Prepare, #protocol{}=P) ->
%%     {ok, [P]}.

%% -- internal: loop,stmt_execute --

%% stmt_execute_pre(Prepare, Args, #protocol{}=P) ->
%%     B = stmt_execute_to_binary(Prepare, Args),
%%     {ok, [Prepare,B,reset(P)]}.

%% stmt_execute_post(<<0>>, #prepare{execute=E}=X, #protocol{}=P) ->
%%     case recv_result(P) of
%%         {ok, [Result,Protocol]} ->
%%             {ok, [X#prepare{result = Result, execute = E+1},Protocol]};
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end;
%% stmt_execute_post(Byte, Prepare, #protocol{}=P) ->
%%     case recv_packed_integer(Byte, P) of
%%         {ok, N, Protocol} ->
%%             stmt_execute_recv_fields(Prepare, N, Protocol);
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end.

%% stmt_execute_recv_fields(#prepare{}=X, N, #protocol{}=P) -> % do CALL, 0=X.filed_count
%%     case recv_until_eof(func_recv_field(P), [], [], P) of
%%         {ok, [Result,Fields,Protocol]} when N == length(Fields) ->
%%             F = myer_protocol_binary:prepare_fields(Fields),
%%             stmt_execute_recv_rows(Result, X#prepare{field_count = N, fields = F}, Protocol);
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end.

%% stmt_execute_recv_rows(#result{status=S}=R, #prepare{execute=E}=X, #protocol{}=P)
%%   when ?IS_SET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
%%     {ok, [X#prepare{result = R, execute = E+1},P]};
%% stmt_execute_recv_rows(_Result, #prepare{fields=F,execute=E}=X, #protocol{}=P) ->
%%     case recv_until_eof(fun myer_protocol_binary:recv_row/3, [F], [], P) of
%%         {ok, [Result,Rows,Protocol]} ->
%%             {ok, [F,Rows,X#prepare{result = Result, execute = E+1},Protocol]};
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end.

%% -- internal: loop,stmt_fetch --

%% stmt_fetch_pre(#prepare{stmt_id=S,prefetch_rows=R}=X, #protocol{}=P) ->
%%     B = <<?COM_STMT_FETCH, S:32/little, R:32/little>>,
%%     {ok, [X,B,reset(P)]}.

%% stmt_fetch_post(#prepare{fields=F}=X, #protocol{}=P) ->
%%     case recv_until_eof(fun myer_protocol_binary:recv_row/3, [F], [], P) of
%%         {ok, [Result,Rows,Protocol]} ->
%%             {ok, [F,Rows,X#prepare{result = Result},Protocol]};
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end.

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

%% -- internal: loop,stmt_reset --

%% stmt_reset_pre(#prepare{stmt_id=S}=X, #protocol{}=P) ->
%%     {ok, [X,<<?COM_STMT_RESET,S:32/little>>,reset(P)]}.

%% stmt_reset_post(<<0>>, #prepare{}=X, #protocol{}=P) ->
%%     case recv_result(P) of
%%         {ok, [Result,Protocol]} ->
%%             {ok, [X#prepare{result = Result},Protocol]};
%%         {error, Reason, Protocol} ->
%%             {error, Reason, Protocol}
%%     end.

%% -----------------------------------------------------------------------------
%% << include/mysql_com.h : CLIENT_ALL_FLAGS
%% -----------------------------------------------------------------------------
default_caps(true) ->
    default_caps(?CLIENT_COMPRESS);
default_caps(false) ->
    default_caps(0);
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

%% pack_binary(<<>>) ->
%%     <<251>>;
pack_binary(Binary) ->
    B = pack_integer(size(Binary)),
    <<B/binary, Binary/binary>>.

pack_integer(Value) when Value < 251      -> <<Value>>;
pack_integer(Value) when Value < 65536    -> <<252, Value:16/little>>;
pack_integer(Value) when Value < 16777216 -> <<253, Value:24/little>>;
pack_integer(Value)                       -> <<254, Value:64/little>>.

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
recv_eof(#result{warning_count=undefined}=R, Term, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_eof(R#result{warning_count = U}, Term, Caps, H);
recv_eof(#result{warning_count=undefined}=R, Term, Caps, Handle) ->
    recv_eof(R#result{warning_count = 0}, Term, Caps, Handle);
recv_eof(#result{status=undefined}=R, Term, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_eof(R#result{status = U}, Term, Caps, H);
recv_eof(#result{status=undefined}=R, Term, Caps, Handle) ->
    recv_eof(R#result{status = 0}, Term, Caps, Handle);
recv_eof(#result{}=R, Term, _Caps, Handle) ->
    {ok, [R,Term,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_server_handshake_packet/3
%%    sql/auth/sql_authentication.cc : send_server_handshake_packet/3
%% -----------------------------------------------------------------------------
recv_handshake(#handshake{version=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv(<<0>>, Caps, Handle),
    recv_handshake(R#handshake{version = binary_to_version(B)}, Caps, H);
recv_handshake(#handshake{tid=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(4, Caps, Handle),
    recv_handshake(R#handshake{tid = U}, Caps, H);
recv_handshake(#handshake{seed1=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv(<<0>>, Caps, Handle),
    recv_handshake(R#handshake{seed1 = B}, Caps, H);
recv_handshake(#handshake{caps1=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_handshake(R#handshake{caps1 = U}, Caps, H);
recv_handshake(#handshake{charset=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(1, Caps, Handle),
    recv_handshake(R#handshake{charset = U}, Caps, H);
recv_handshake(#handshake{status=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_handshake(R#handshake{status = U}, Caps, H);
recv_handshake(#handshake{version=V,caps1=C,caps2=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_handshake(R#handshake{caps2 = U}, Caps, H);
recv_handshake(#handshake{caps2=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{caps2 = 0}, Caps, Handle);
recv_handshake(#handshake{version=V,caps1=C,length=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(1, Caps, Handle),
    recv_handshake(R#handshake{length = U}, Caps, H);
recv_handshake(#handshake{length=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{length = 8}, Caps, Handle);   % 8?, TODO
recv_handshake(#handshake{version=V,caps1=C,reserved=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv(10, Caps, Handle),
    recv_handshake(R#handshake{reserved = B}, Caps, H); % "always 0"
recv_handshake(#handshake{reserved=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv(13, Caps, Handle),
    recv_handshake(R#handshake{reserved = B}, Caps, H); % "always 0"?
recv_handshake(#handshake{version=V,caps1=C,seed2=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv(<<0>>, Caps, Handle),
    recv_handshake(R#handshake{seed2 = B}, Caps, H);
recv_handshake(#handshake{seed2=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{seed2 = <<>>}, Caps, Handle);
recv_handshake(#handshake{version=V,caps1=C,plugin=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv(<<0>>, Caps, Handle),
    recv_handshake(R#handshake{plugin = B}, Caps, H);
recv_handshake(#handshake{plugin=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{plugin = <<"mysql_native_password">>}, Caps, Handle);
recv_handshake(#handshake{caps1=C1,caps2=C2,seed1=S1,seed2=S2}=R, Caps, Handle) ->
    {ok, [R#handshake{caps = Caps band ((C2 bsl 16) bor C1), seed = <<S1/binary,S2/binary>>}, Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_plugin_request_packet/3
%%    sql/auth/sql_authentication.cc : send_plugin_request_packet/3
%% -----------------------------------------------------------------------------
recv_plugin(#plugin{name=undefined}=P, Handshake, Caps, Handle) ->
    {ok, B, H} = recv(<<0>>, Caps, Handle),
    recv_plugin(P#plugin{name = B}, Handshake, Caps, H);
recv_plugin(#plugin{}=P, Handshake, _Caps, Handle) ->
    {ok, [P,Handshake,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_prepare.cc : send_prep_stmt/2
%% -----------------------------------------------------------------------------
recv_prepare(#prepare{stmt_id=undefined}=P, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(4, Caps, Handle),
    recv_prepare(P#prepare{stmt_id=U}, Caps, H);
recv_prepare(#prepare{field_count=undefined}=P, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_prepare(P#prepare{field_count=U}, Caps, H);
recv_prepare(#prepare{param_count=undefined}=P, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_prepare(P#prepare{param_count=U}, Caps, H);
recv_prepare(#prepare{reserved=undefined}=P, Caps, Handle) ->
    {ok, B, H} = recv(1, Caps, Handle), % <<0>>
    recv_prepare(P#prepare{reserved=B}, Caps, H);
recv_prepare(#prepare{warning_count=undefined}=P, Caps, Handle) -> % < 5.0, undefined?
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_prepare(P#prepare{warning_count=U}, Caps, H);
recv_prepare(#prepare{}=P, _Caps, Handle) ->
    {ok, [P,Handle]}.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_error_packet/4
%%    sql/protocol_classic.cc : net_send_error_packet/7
%% -----------------------------------------------------------------------------
recv_reason(#reason{errno=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_reason(R#reason{errno = U}, Caps, H);
recv_reason(#reason{reserved=undefined}=R, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv(1, Caps, Handle), % <<$#>>
    recv_reason(R#reason{reserved = B}, Caps, H);
recv_reason(#reason{state=undefined}=R, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv(5, Caps, Handle),
    recv_reason(R#reason{state = B}, Caps, H);
recv_reason(#reason{message=undefined}=R, Caps, Handle)->
    {ok, B, H} = recv(remains(Handle), Caps, Handle),
    recv_reason(R#reason{message = B}, Caps, H);
recv_reason(#reason{}=R, _Caps, Handle) ->
    {error, R, Handle}. % != ok

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_ok/6
%%    sql/protocol_classic.cc : net_send_ok/7
%% -----------------------------------------------------------------------------
recv_result(#result{affected_rows=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_packed_unsigned(Caps, Handle),
    recv_result(R#result{affected_rows = U}, Caps, H);
recv_result(#result{insert_id=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_packed_unsigned(Caps, Handle),
    recv_result(R#result{insert_id = U}, Caps, H);
recv_result(#result{status=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_result(R#result{status = U}, Caps, H);
recv_result(#result{warning_count=undefined}=R, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Caps, Handle),
    recv_result(R#result{warning_count = U}, Caps, H);
recv_result(#result{message=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv(remains(Handle), Caps, Handle),
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

stmt_execute_to_binary(#prepare{stmt_id=S,flags=F,execute=_E}, Args) ->
    W = (length(Args) + 7) div 8,
    {T, D, U} = stmt_execute_fold_args(Args, W, 0, [], [], 0),
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
