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
-export([real_query/2, refresh/2, select_db/2]).
-export([stmt_prepare/2, stmt_close/2, stmt_execute/3, stmt_fetch/2, stmt_reset/2]).
-export([next_result/1, stmt_next_result/2]).

%% -- private --
-export([binary_to_float/2]).
-export([recv/2, recv_packed_binary/2]).


-export([connect/1, close/1, auth/1]).
-export([ping/1, stat/1]).

-type(socket() :: tuple()). % TODO

%% == private ==

-spec connect([term()]) -> {ok,handshake(),socket()}|{error,_}|{error,_,socket()}.
connect(Args) ->
    loop(Args, [fun connect_pre/4, fun recv_status2/3, fun connect_post/4]).

-spec close([term()]) -> ok|{error,_,socket()}.
close(Args) ->
    loop(Args, [fun close_pre/2, fun send2/3, fun close_post/2]).

-spec auth([term()]) -> {ok,result(),socket()}|{error,_,socket()}.
auth(Args) ->
    case loop(Args, [fun auth_pre/5, fun send2/4, fun recv_status2/3, fun auth_post/4]) of
        {ok, #plugin{name=N}, #handshake{}=H, Socket} ->
            auth_alt([lists:nth(3,Args),H#handshake{plugin = N},Socket]);
        {ok, #result{}=R, Socket}->
            {ok, R, Socket};
        {error, Reason, Socket} ->
            {error, Reason, Socket}
    end.

auth_alt(Args) ->
    loop(Args, [fun auth_alt_pre/4, fun send2/4, fun recv_status2/3, fun auth_alt_post/4]).


-spec ping([term()]) -> {ok,result(),socket()}|{error,_,socket()}.
ping(Args) ->
    loop(Args, [fun ping_pre/2, fun send2/3, fun recv_status2/2, fun recv_result2/3]).


-spec stat([term()]) -> {ok,binary(),socket()}|{error,_,socket()}.
stat(Args) ->
    loop(Args, [fun stat_pre/2, fun send2/3, fun recv_status2/2, fun stat_post/3]).

%% == internal ==

loop(Args, []) ->
    list_to_tuple([ok|Args]);
loop(Args, [H|T]) ->
    case apply(H, Args) of
        {ok, List} ->
            loop(List, T);
        {error, Reason} -> % connect_pre
            {error, Reason};
        {error, Reason, Socket} ->
            {error, Reason, Socket}
    end.

recv(Term, Caps, Socket) ->
    myer_socket:recv(Socket, Term, ?IS_SET(Caps,?CLIENT_COMPRESS)).

recv_error(Caps, Socket) ->
    recv_reason(#reason{}, Caps, Socket).

recv_packed_unsigned(Caps, S) ->
    case recv(1, Caps, S) of
        {ok, <<254>>, Socket} ->
            recv_unsigned(8, Caps, Socket);
        {ok, <<253>>, Socket} ->
            recv_unsigned(3, Caps, Socket);
        {ok, <<252>>, Socket} ->
            recv_unsigned(2, Caps, Socket);
        {ok, <<251>>, Socket} ->
            {ok, null, Socket};
        {ok, <<Int>>, Socket} ->
            {ok, Int, Socket};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_result2(Caps, Socket) ->
    recv_result(#result{}, Caps, Socket).

recv_result2(<<0>>, Caps, Socket) ->
    recv_result(#result{}, Caps, Socket).

recv_status2(Caps, S) ->
    case recv(1, Caps, S) of
        {ok, <<255>>, Socket} ->
            recv_error(Caps, Socket);
        {ok, Byte, Socket} ->
            {ok, [Byte,Caps,Socket]};
        {error, Reason} ->
            {error, Reason, S}
    end.

recv_status2(Term, Caps, S) ->
    case recv(1, Caps, S) of
        {ok, <<255>>, Socket} ->
            recv_error(Caps, Socket);
        {ok, Byte, Socket} ->
            {ok, [Byte,Term,Caps,Socket]};
        {error, Reason} ->
            {error, Reason, S}
    end.

recv_unsigned(Length, Caps, Socket) -> % buffered
    {ok, B, S} = recv(Length, Caps, Socket),
    {ok, binary:decode_unsigned(B,little), S}.

remains(Socket) ->
    myer_socket:remains(Socket).

reset2(Socket) ->
    myer_socket:reset(Socket).

send2(Binary, Caps, S) ->
    case myer_socket:send(S, Binary, ?IS_SET(Caps,?CLIENT_COMPRESS)) of
        {ok, Socket} ->
            {ok, [Caps,Socket]};
        {error, Reason} ->
            {error, Reason}
    end.

send2(Term, Binary, Caps, S) ->
    case myer_socket:send(S, Binary, ?IS_SET(Caps,?CLIENT_COMPRESS)) of
        {ok, Socket} ->
            {ok, [Term,Caps,Socket]};
        {error, Reason} ->
            {error, Reason}
    end.

%% -- internal: connect --

connect_pre(Address, Port, MaxLength, Timeout) ->
    case myer_socket:connect(Address, Port, MaxLength, timer:seconds(Timeout)) of
        {ok, Socket} ->
            {ok, [MaxLength,default_caps(false),Socket]};
        {error, Reason} ->
            {error, Reason}
    end.

connect_post(<<10>>, MaxLength, Caps, Socket) -> % "always 10"
    recv_handshake(#handshake{maxlength = MaxLength}, Caps, Socket). % buffered

%% -- internal: close --

close_pre(Socket, Caps) ->
    {ok, [<<?COM_QUIT>>,Caps,Socket]}.

close_post(_Caps, Socket) ->
    _ = myer_socket:close(Socket),
    {ok, [undefined]}.

%% -- internal: auth --

auth_pre(Socket, User, Password, <<>>, #handshake{caps=C}=H) ->
    Handshake = H#handshake{caps = (C bxor ?CLIENT_CONNECT_WITH_DB)},
    {ok, [Handshake,auth_to_binary(User,Password,<<>>,Handshake),C,Socket]};
auth_pre(Socket, User, Password, Database, #handshake{caps=C}=H) ->
    Handshake = H#handshake{caps = (C bxor ?CLIENT_NO_SCHEMA)},
    {ok, [Handshake,auth_to_binary(User,Password,Database,Handshake),C,Socket]}.

auth_post(<<254>>, Handshake, Caps, Socket) ->
    recv_plugin(#plugin{}, Handshake, Caps, Socket);
auth_post(<<0>>, _Handshake, Caps, Socket) ->
    recv_result2(Caps, Socket).

auth_alt_pre(Socket, Caps, Password, #handshake{seed=S,plugin=A}=H) ->
    Scrambled = myer_auth:scramble(Password, S, A),
    {ok, [<<Scrambled/binary,0>>,H,Caps, Socket]}.

auth_alt_post(<<0>>, _Handshake, Caps, Socket) ->
    recv_result2(Caps, Socket).


%% -- internal: ping --

ping_pre(Socket, Caps) ->
    {ok, [<<?COM_PING>>,Caps,reset2(Socket)]}.

%% -- internal: stat --

stat_pre(Socket, Caps) ->
    {ok, [<<?COM_STATISTICS>>,Caps,reset2(Socket)]}.

stat_post(Byte, Caps, S) ->
    case recv(remains(S), Caps, S) of
        {ok, Binary, Socket} ->
            {ok, [list_to_binary([Byte,Binary]),Socket]};
        {error, Reason} ->
            {error, Reason, S}
    end.





%% == public ==

-spec real_query(protocol(),binary())
                -> {ok,result(),protocol()}|
                   {ok,{[field()],[term()],result()},protocol()}|{error,_,protocol()}.
real_query(#protocol{handle=H}=P, Query)
  when undefined =/= H, is_binary(Query) ->
    loop([Query,P], [fun real_query_pre/2, fun send/2, fun recv_status/1, fun real_query_post/2]).

-spec refresh(protocol(),integer()) -> {ok,result(),protocol()}|{error,_,protocol()}.
refresh(#protocol{handle=H}=P, Option)
  when undefined =/= H ->
    loop([Option,P], [fun refresh_pre/2, fun send/2, fun recv_status/1, fun recv_result/2]).

-spec select_db(protocol(),binary()) -> {ok,result(),protocol()}|{error,_,protocol()}.
select_db(#protocol{handle=H}=P, Database)
  when undefined =/= H, is_binary(Database) ->
    loop([Database,P], [fun select_db_pre/2, fun send/2, fun recv_status/1, fun recv_result/2]).

-spec stmt_prepare(protocol(),binary()) -> {ok,prepare(),protocol()}|{error,_,protocol()}.
stmt_prepare(#protocol{handle=H}=P, Query)
  when undefined =/= H, is_binary(Query) ->
    loop([Query,P], [fun stmt_prepare_pre/2, fun send/2, fun recv_status/1, fun stmt_prepare_post/2]).

-spec stmt_close(protocol(),prepare()) -> {ok,undefined,protocol()}|{error,_,protocol()}.
stmt_close(#protocol{handle=H}=P, #prepare{}=X)
  when undefined =/= H ->
    loop([X,P], [fun stmt_close_pre/2, fun send/3, fun stmt_close_post/2]).

-spec stmt_execute(protocol(),prepare(),[term()])
                  -> {ok,prepare(),protocol()}|
                     {ok,{[field()],[term()],prepare()},protocol()}|{error,_,protocol()}.
stmt_execute(#protocol{handle=H}=P, #prepare{param_count=N}=X, Args)
  when undefined =/= H, is_list(Args), N == length(Args) ->
    loop([X,Args,P], [fun stmt_execute_pre/3, fun send/3, fun recv_status/2, fun stmt_execute_post/3]).

-spec stmt_fetch(protocol(),prepare())
                -> {ok,prepare(),protocol()}|
                   {ok,{[field()],[term()],prepare()},protocol()}|{error,_,protocol()}.
stmt_fetch(#protocol{handle=H}=P, #prepare{result=R}=X)
  when undefined =/= H ->
    case R#result.status of
        S when ?IS_SET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
            loop([X,P], [fun stmt_fetch_pre/2, fun send/3, fun stmt_fetch_post/2]);
        _ ->
            {ok, X, P}
    end.

-spec stmt_reset(protocol(),prepare()) -> {ok,prepare(),protocol()}|{error,_,protocol()}.
stmt_reset(#protocol{handle=H}=P, #prepare{}=X)
  when undefined =/= H ->
    loop([X,P], [fun stmt_reset_pre/2, fun send/3, fun recv_status/2, fun stmt_reset_post/3]).

-spec next_result(protocol())
                 -> {ok,result(),protocol()}|
                    {ok,{[field()],[term()],result()},protocol()}|{error,_,protocol()}.
next_result(#protocol{handle=H}=P)
  when undefined =/= H ->
    loop([P], [fun next_result_pre/1, fun recv_status/1, fun real_query_post/2]).

-spec stmt_next_result(protocol(),prepare())
                      -> {ok,prepare()}|{ok,[field()],[term()],prepare(),protocol()}|{error,_, protocol()}.
stmt_next_result(#protocol{handle=H}=P, #prepare{}=X)
  when undefined =/= H ->
    loop([X,P], [fun stmt_next_result_pre/2, fun recv_status/2, fun stmt_next_result_post/3]).


%% == private ==

-spec binary_to_float(binary(),non_neg_integer()) -> float().
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

-spec recv(protocol(),term()) -> {ok,binary(),protocol()}|{error,_,protocol()}.
recv(#protocol{handle=H,caps=C}=P, Term) ->
    case myer_socket:recv(H, Term, ?IS_SET(C,?CLIENT_COMPRESS)) of
        {ok, Binary, Handle} ->
            {ok, Binary, P#protocol{handle = Handle}};
        {error, Reason} ->
            {error, Reason, P}
    end.

-spec recv_packed_binary(undefined|binary(),protocol())
                        -> {ok,null|binary(),protocol()}|{error,_,protocol()}.
recv_packed_binary(Byte, #protocol{}=P) ->
    case recv_packed_integer(Byte, P) of
        {ok, null, Protocol} ->
            {ok, null, Protocol};
        {ok, 0, Protocol} ->
            {ok, <<>>, Protocol};
        {ok, Length, Protocol} ->
            recv(Protocol, Length)
    end.

%% == internal ==

binary_to_version(Binary) ->
    F = fun(E) -> try binary_to_integer(E,10) catch _:_ -> E end end,
    L = binary:split(<<Binary/binary,".0.0">>, [<<$.>>,<<$->>], [global]),
    lists:map(F, lists:sublist(L,3)).

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
         ?CLIENT_SECURE_CONNECTION, %?CLIENT_RESERVED2
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

func_recv_field(#protocol{caps=C})
  when ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    fun myer_protocol_text:recv_field_41/2;
func_recv_field(_Protocol) ->
    fun myer_protocol_text:recv_field/2.

reset(#protocol{handle=H}=P) ->
    P#protocol{handle = myer_socket:reset(H)}.

zreset(#protocol{handle=H}=P) ->
    P#protocol{handle = myer_socket:zreset(H)}.

%% -- internal: loop,next_result --

next_result_pre(#protocol{}=P) ->
    {ok, [zreset(P)]}.

%% -- internal: loop,real_query --

real_query_pre(Query, #protocol{}=P) ->
    {ok, [<<?COM_QUERY,Query/binary>>,reset(P)]}.

real_query_post(<<0>>, #protocol{}=P) ->
    recv_result(P);
real_query_post(Byte, #protocol{}=P) ->
    case recv_packed_integer(Byte, P) of
        {ok, N, Protocol} ->
            real_query_recv_fields(N, Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

real_query_recv_fields(N, #protocol{}=P) ->
    case recv_until_eof(func_recv_field(P), [], [], P) of
        {ok, [_Result,Fields,Protocol]} when N == length(Fields) ->
            real_query_recv_rows(Fields, Protocol);
        {error, Reason, P} ->
            {error, Reason, P}
    end.

real_query_recv_rows(Fields, #protocol{}=P) ->
    case recv_until_eof(fun myer_protocol_text:recv_row/3, [Fields], [], P) of
        {ok, [Result,Rows,Protocol]} ->
            {ok, [Fields,Rows,Result,Protocol]};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- internal: loop,refresh --

refresh_pre(Options, #protocol{}=P) ->
    {ok, [<<?COM_REFRESH,Options>>,reset(P)]}.

%% -- internal: loop,select_db --

select_db_pre(Database, #protocol{}=P) ->
    {ok, [<<?COM_INIT_DB,Database/binary>>,reset(P)]}.

%% -- internal: loop,stmt_close --

stmt_close_pre(#prepare{stmt_id=S}=X, #protocol{}=P) ->
    {ok, [X,<<?COM_STMT_CLOSE,S:32/little>>,reset(P)]}.

stmt_close_post(_Prepare, #protocol{}=P) ->
    {ok, [P]}.

%% -- internal: loop,stmt_execute --

stmt_execute_pre(Prepare, Args, #protocol{}=P) ->
    B = stmt_execute_to_binary(Prepare, Args),
    {ok, [Prepare,B,reset(P)]}.

stmt_execute_post(<<0>>, #prepare{execute=E}=X, #protocol{}=P) ->
    case recv_result(P) of
        {ok, [Result,Protocol]} ->
            {ok, [X#prepare{result = Result, execute = E+1},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end;
stmt_execute_post(Byte, Prepare, #protocol{}=P) ->
    case recv_packed_integer(Byte, P) of
        {ok, N, Protocol} ->
            stmt_execute_recv_fields(Prepare, N, Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

stmt_execute_recv_fields(#prepare{}=X, N, #protocol{}=P) -> % do CALL, 0=X.filed_count
    case recv_until_eof(func_recv_field(P), [], [], P) of
        {ok, [Result,Fields,Protocol]} when N == length(Fields) ->
            F = myer_protocol_binary:prepare_fields(Fields),
            stmt_execute_recv_rows(Result, X#prepare{field_count = N, fields = F}, Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

stmt_execute_recv_rows(#result{status=S}=R, #prepare{execute=E}=X, #protocol{}=P)
  when ?IS_SET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
    {ok, [X#prepare{result = R, execute = E+1},P]};
stmt_execute_recv_rows(_Result, #prepare{fields=F,execute=E}=X, #protocol{}=P) ->
    case recv_until_eof(fun myer_protocol_binary:recv_row/3, [F], [], P) of
        {ok, [Result,Rows,Protocol]} ->
            {ok, [F,Rows,X#prepare{result = Result, execute = E+1},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- internal: loop,stmt_fetch --

stmt_fetch_pre(#prepare{stmt_id=S,prefetch_rows=R}=X, #protocol{}=P) ->
    B = <<?COM_STMT_FETCH, S:32/little, R:32/little>>,
    {ok, [X,B,reset(P)]}.

stmt_fetch_post(#prepare{fields=F}=X, #protocol{}=P) ->
    case recv_until_eof(fun myer_protocol_binary:recv_row/3, [F], [], P) of
        {ok, [Result,Rows,Protocol]} ->
            {ok, [F,Rows,X#prepare{result = Result},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- internal: loop,stmt_next_result --

stmt_next_result_pre(#prepare{}=X, #protocol{}=P) ->
    {ok, [X,zreset(P)]}.

stmt_next_result_post(<<0>>, #prepare{execute=E}=X, #protocol{}=P) ->
    case recv_result(P) of
        {ok, [Result,Protocol]} ->
            {ok, [X#prepare{result = Result, execute = E+1},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- internal: loop,stmt_prepare --

stmt_prepare_pre(Query, #protocol{}=P) ->
    {ok, [<<?COM_STMT_PREPARE,Query/binary>>,reset(P)]}.

stmt_prepare_post(<<0>>, #protocol{}=P) ->
    case recv(P, 0) of
        {ok, Binary, Protocol} ->
            stmt_prepare_recv_params(binary_to_prepare(Binary), Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

stmt_prepare_recv_params(#prepare{param_count=0}=X, #protocol{}=P) ->
    stmt_prepare_recv_fields(undefined, X#prepare{params = []}, P);
stmt_prepare_recv_params(#prepare{param_count=N}=X, #protocol{}=P) ->
    case recv_until_eof(func_recv_field(P), [], [], P) of
        {ok, [Result,Params,Protocol]} when N == length(Params)->
            stmt_prepare_recv_fields(Result, X#prepare{params = Params}, Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

stmt_prepare_recv_fields(Result, #prepare{field_count=0}=X, #protocol{}=P) ->
    {ok, [X#prepare{result = Result},P]};
stmt_prepare_recv_fields(_Result, #prepare{field_count=N}=X, #protocol{}=P) ->
    case recv_until_eof(func_recv_field(P), [], [], P) of
        {ok, [Result,Fields,Protocol]} when N == length(Fields) ->
            {ok, [X#prepare{fields = Fields, result = Result}, Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- internal: loop,stmt_reset --

stmt_reset_pre(#prepare{stmt_id=S}=X, #protocol{}=P) ->
    {ok, [X,<<?COM_STMT_RESET,S:32/little>>,reset(P)]}.

stmt_reset_post(<<0>>, #prepare{}=X, #protocol{}=P) ->
    case recv_result(P) of
        {ok, [Result,Protocol]} ->
            {ok, [X#prepare{result = Result},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- internal: network --

recv_eof(Term, #protocol{caps=C}=P) ->
    case recv(P, 0) of
        {ok, Binary, Protocol} ->
            {ok, [binary_to_eof(C,Binary),Term,Protocol]};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_error(#protocol{caps=C}=P) ->
    case recv(P, 0) of
        {ok, Binary, Protocol} ->
            {error, binary_to_reason(C,Binary,0,byte_size(Binary)), Protocol};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

recv_result(#protocol{caps=C}=P) ->
    case recv(P, 0) of
        {ok, Binary, Protocol} ->
            {ok, [binary_to_result(C,Binary,0,byte_size(Binary)),Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

recv_result(<<0>>, Protocol) ->
    recv_result(Protocol).

recv_status(#protocol{}=P) ->
    case recv(P, 1) of
        {ok, <<255>>, Protocol} ->
            recv_error(Protocol);
        {ok, Byte, Protocol} ->
            {ok, [Byte,Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

recv_status(Term, #protocol{}=P) ->
    case recv(P, 1) of
        {ok, <<255>>, Protocol} ->
            recv_error(Protocol);
        {ok, Byte, Protocol} ->
            {ok, [Byte,Term,Protocol]};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_unsigned(Length, #protocol{}=P) ->
    case recv(P, Length) of
        {ok, Binary, Protocol} ->
            {ok, [binary:decode_unsigned(Binary,little),Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

recv_until_eof(Func, Args, List, #protocol{}=P) ->
    case recv(P, 1) of
        {ok, <<255>>, Protocol} ->
            recv_error(Protocol);
        {ok, <<254>>, Protocol} ->
            recv_eof(lists:reverse(List), Protocol);
        {ok, Byte, Protocol} ->
            recv_until_eof(Func, Args, List, Byte, Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

recv_until_eof(Func, Args, List, Byte, #protocol{}=P) ->
    case apply(Func, [P|[Byte|Args]]) of % TODO
        {ok, Term, Protocol} ->
            recv_until_eof(Func, Args, [Term|List], Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

send(Binary, #protocol{handle=H,caps=C}=P) ->
    case myer_socket:send(H, Binary, ?IS_SET(C,?CLIENT_COMPRESS)) of
        {ok, Handle} ->
            {ok, [P#protocol{handle = Handle}]};
        {error, Reason} ->
            {error, Reason, P}
    end.

send(Term, Binary, #protocol{handle=H,caps=C}=P) ->
    case myer_socket:send(H, Binary, ?IS_SET(C,?CLIENT_COMPRESS)) of
        {ok, Handle} ->
            {ok, [Term,P#protocol{handle = Handle}]};
        {error, Reason} ->
            {error, Reason, P}
    end.

%% -- internal: sql* --

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

unpack_binary(Binary, Start, Length) ->
    case unpack_integer(Binary, Start, Length) of
        {null, S, L} ->
            {null, S, L};
        {Len, S, L} ->
            {binary_part(Binary,S,Len), S+Len, L-Len}
    end.

unpack_integer(_Binary, Start, 0) ->
    {0, Start, 0};
unpack_integer(Binary, Start, Length) ->
    case binary_part(Binary, Start, 1) of
        <<254>> -> <<L:64/little>> = binary_part(Binary, Start+1, 8), {L, Start+9, Length-9};
        <<253>> -> <<L:24/little>> = binary_part(Binary, Start+1, 4), {L, Start+5, Length-5};
        <<252>> -> <<L:16/little>> = binary_part(Binary, Start+1, 2), {L, Start+3, Length-3};
        <<251>> -> {null, Start+1, Length-1};
        <<L>> -> {L, Start+1, Length-1}
    end.

recv_packed_integer(undefined, Protocol) ->
    case recv(Protocol, 1) of
        {ok, Byte, #protocol{}=P} ->
            recv_packed_integer(Byte, P);
        {error, Reason, P} ->
            {error, Reason, P}
    end;
recv_packed_integer(<<254>>, Protocol) -> recv_unsigned(8, Protocol);
recv_packed_integer(<<253>>, Protocol) -> recv_unsigned(3, Protocol);
recv_packed_integer(<<252>>, Protocol) -> recv_unsigned(2, Protocol);
recv_packed_integer(<<251>>, Protocol) -> {ok, null, Protocol};
recv_packed_integer(<<Int>>, Protocol) -> {ok, Int, Protocol}.

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

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc : net_send_eof/3
%% -----------------------------------------------------------------------------
binary_to_eof(_Caps, <<>>) -> % FORCE: 4.1.25, TODO
    #result{status = 0, warning_count = 0};
binary_to_eof(Caps, Binary) % eof -> #result{}
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    <<W:16/little, S:16/little>> = Binary,
    #result{status = S, warning_count = W}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_server_handshake_packet/3
%%    sql/auth/sql_authentication.cc : send_server_handshake_packet/3
%% -----------------------------------------------------------------------------
recv_handshake(#handshake{version=undefined}=H, Caps, Socket) ->
    {ok, B, S} = recv(<<0>>, Caps, Socket),
    recv_handshake(H#handshake{version = binary_to_version(B)}, Caps, S);
recv_handshake(#handshake{tid=undefined}=H, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(4, Caps, Socket),
    recv_handshake(H#handshake{tid = U}, Caps, S);
recv_handshake(#handshake{seed1=undefined}=H, Caps, Socket) ->
    {ok, B, S} = recv(<<0>>, Caps, Socket),
    recv_handshake(H#handshake{seed1 = B}, Caps, S);
recv_handshake(#handshake{caps1=undefined}=H, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_handshake(H#handshake{caps1 = U}, Caps, S);
recv_handshake(#handshake{charset=undefined}=H, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(1, Caps, Socket),
    recv_handshake(H#handshake{charset = U}, Caps, S);
recv_handshake(#handshake{status=undefined}=H, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_handshake(H#handshake{status = U}, Caps, S);
recv_handshake(#handshake{version=V,caps1=C,caps2=undefined}=H, Caps, Socket)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_handshake(H#handshake{caps2 = U}, Caps, S);
recv_handshake(#handshake{caps2=undefined}=H, Caps, Socket) ->
    recv_handshake(H#handshake{caps2 = 0}, Caps, Socket);
recv_handshake(#handshake{version=V,caps1=C,length=undefined}=H, Caps, Socket)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(1, Caps, Socket),
    recv_handshake(H#handshake{length = U}, Caps, S);
recv_handshake(#handshake{length=undefined}=H, Caps, Socket) ->
    recv_handshake(H#handshake{length = 8}, Caps, Socket);   % 8?, TODO
recv_handshake(#handshake{version=V,caps1=C,reserved=undefined}=H, Caps, Socket)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(10, Caps, Socket),
    recv_handshake(H#handshake{reserved = B}, Caps, S); % "always 0"
recv_handshake(#handshake{reserved=undefined}=H, Caps, Socket) ->
    {ok, B, S} = recv(13, Caps, Socket),
    recv_handshake(H#handshake{reserved = B}, Caps, S); % "always 0"?
recv_handshake(#handshake{version=V,caps1=C,seed2=undefined}=H, Caps, Socket)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(<<0>>, Caps, Socket),
    recv_handshake(H#handshake{seed2 = B}, Caps, S);
recv_handshake(#handshake{seed2=undefined}=H, Caps, Socket) ->
    recv_handshake(H#handshake{seed2 = <<>>}, Caps, Socket);
recv_handshake(#handshake{version=V,caps1=C,plugin=undefined}=H, Caps, Socket)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(<<0>>, Caps, Socket),
    recv_handshake(H#handshake{plugin = B}, Caps, S);
recv_handshake(#handshake{plugin=undefined}=H, Caps, Socket) ->
    recv_handshake(H#handshake{plugin = <<"mysql_native_password">>}, Caps, Socket);
recv_handshake(#handshake{caps1=C1,caps2=C2,seed1=S1,seed2=S2}=H, Caps, Socket) ->
    {ok, [H#handshake{caps = Caps band ((C2 bsl 16) bor C1), seed = <<S1/binary, S2/binary>>}, Socket]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_plugin_request_packet/3
%%    sql/auth/sql_authentication.cc : send_plugin_request_packet/3
%% -----------------------------------------------------------------------------
recv_plugin(#plugin{name=undefined}=P, Handshake, Caps, Socket) ->
    {ok, B, S} = recv(<<0>>, Caps, Socket),
    recv_plugin(P#plugin{name = B}, Handshake, Caps, S);
recv_plugin(#plugin{}=P, Handshake, _Caps, Socket) ->
    {ok, [P,Handshake,Socket]}.

%% -----------------------------------------------------------------------------
%% << sql/sql_prepare.cc : send_prep_stmt/2
%% -----------------------------------------------------------------------------
binary_to_prepare(Binary) -> % < 5.0.0, warning_count=undefined
    <<S:32/little, F:16/little, P:16/little, 0, W:16/little>> = Binary,
    #prepare{stmt_id = S, field_count = F, param_count = P, warning_count = W,
             flags = ?CURSOR_TYPE_NO_CURSOR, prefetch_rows = 1, execute = 0}.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_error_packet/4
%%    sql/protocol_classic.cc : net_send_error_packet/7
%% -----------------------------------------------------------------------------
recv_reason(#reason{errno=undefined}=R, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_reason(R#reason{errno = U}, Caps, S);
recv_reason(#reason{reserved=undefined}=R, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(1, Caps, Socket), % <<$#>>
    recv_reason(R#reason{reserved = B}, Caps, S);
recv_reason(#reason{state=undefined}=R, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(5, Caps, Socket),
    recv_reason(R#reason{state = B}, Caps, S);
recv_reason(#reason{message=undefined}=R, Caps, Socket)->
    {ok, B, S} = recv(remains(Socket), Caps, Socket),
    recv_reason(R#reason{message = B}, Caps, S);
recv_reason(#reason{}=R, _Caps, Socket) ->
    {error, R, Socket}. % != ok


binary_to_reason(Caps, Binary, Start, Length)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    <<E:16/little>> = binary_part(Binary, Start, 2),
    <<$#>> = binary_part(Binary, Start+2, 1),
    S = binary_part(Binary, Start+3, 5),
    M = binary_part(Binary, Start+8, Length-8),
    #reason{errno = E, state = S, message = M};
binary_to_reason(_Caps, Binary, Start, Length) ->
    <<E:16/little>> = binary_part(Binary, Start, 2),
    M = binary_part(Binary, Start+2, Length-2),
    #reason{errno = E, message = M}.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc (< 5.7) : net_send_ok/6
%%    sql/protocol_classic.cc : net_send_ok/7
%% -----------------------------------------------------------------------------
recv_result(#result{affected_rows=undefined}=R, Caps, Socket) ->
    {ok, U, S} = recv_packed_unsigned(Caps, Socket),
    recv_result(R#result{affected_rows = U}, Caps, S);
recv_result(#result{insert_id=undefined}=R, Caps, Socket) ->
    {ok, U, S} = recv_packed_unsigned(Caps, Socket),
    recv_result(R#result{insert_id = U}, Caps, S);
recv_result(#result{status=undefined}=R, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_result(R#result{status = U}, Caps, S);
recv_result(#result{warning_count=undefined}=R, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_result(R#result{warning_count = U}, Caps, S);
recv_result(#result{message=undefined}=R, Caps, Socket) ->
    {ok, B, S} = recv(remains(Socket), Caps, Socket),
    recv_result(R#result{message = B}, Caps, S);
recv_result(#result{}=R, _Caps, Socket) ->
    {ok, [R,Socket]}.


binary_to_result(Caps, Binary, Start, Length)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {A, S1, L1} = unpack_integer(Binary, Start, Length),
    {I, S2, L2} = unpack_integer(Binary, S1, L1),
    <<S:16/little, W:16/little>> = binary_part(Binary, S2, 4),
    {M, _, 0} = unpack_binary(Binary, S2+4, L2-4),
    #result{affected_rows = A, insert_id = I, status = S, warning_count = W, message = M};
binary_to_result(_Caps, Binary, Start, Length) ->
    {N, S1, L1} = unpack_integer(Binary, Start, Length),
    {I, S2, L2} = unpack_integer(Binary, S1, L1),
    <<S:16/little>> = binary_part(Binary, S2, 2), % < 4.0 -> S:8 ?
    {M, _, 0} = unpack_binary(Binary, S2+2, L2-2),
    #result{affected_rows = N, insert_id = I, status = S, message = M}.
