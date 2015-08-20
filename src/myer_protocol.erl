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
-export([connect/1, close/1, auth/4, ping/1, stat/1, version/1]).
-export([real_query/2, refresh/2, select_db/2]).
-export([stmt_prepare/2, stmt_close/2, stmt_execute/3, stmt_fetch/2, stmt_reset/2]).
-export([next_result/1, stmt_next_result/2]).

%% -- protected --
-export([binary_to_float/2, binary_to_integer/3]).
-export([recv/2, recv_packed_binary/1, recv_packed_binary/2]).

%% -- private --

-record(handshake, {
	  version :: [non_neg_integer()],
	  tid :: non_neg_integer(),
	  seed :: binary(),
	  caps :: integer(),
	  charset :: non_neg_integer(),
	  status :: integer(),
	  plugin :: binary()
	 }).

-record(plugin, {
          name :: binary()
         }).

%% == public ==

-spec connect([term()]) -> {ok,protocol()}|{error,_}|{error,_,protocol()}.
connect(Args)
  when is_list(Args), 6 =:= length(Args) ->
    loop(Args, [fun connect_pre/6, fun recv_status/1, fun connect_post/2]).

-spec close(protocol()) -> {ok,protocol()}|{error,_}|{error,_,protocol()}.
close(#protocol{handle=undefined}=P) ->
    {ok, P};
close(#protocol{}=P) ->
    loop([P], [fun close_pre/1, fun send/2, fun close_post/1]).

-spec auth(protocol(),binary(),binary(),binary()) -> {ok,result(),protocol()}|{error,_,protocol()}.
auth(#protocol{handle=H}=P, User, Password, Database)
  when undefined =/= H, is_binary(User), is_binary(Password), is_binary(Database) ->
    case loop([User,Password,Database,P],
              [fun auth_pre/4, fun send/2, fun recv_status/1, fun auth_post/2]) of
        {ok, #plugin{}=X, Protocol} ->
            auth_alt([Password,merge(Protocol,X)]);
        {ok, Result, Protocol}->
            {ok, Result, Protocol};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

auth_alt(Args) ->
    loop(Args, [fun auth_alt_pre/2, fun send/2, fun recv_status/1, fun auth_alt_post/2]).

-spec ping(protocol()) -> {ok,result(),protocol()}|{error,_,protocol()}.
ping(#protocol{handle=H}=P)
  when undefined =/= H ->
    loop([P], [fun ping_pre/1, fun send/2, fun recv_status/1, fun recv_result/2]).

-spec stat(protocol()) -> {ok,binary(),protocol()}|{error,_,protocol()}.
stat(#protocol{handle=H}=P)
  when undefined =/= H ->
    loop([P], [fun stat_pre/1, fun send/2, fun recv_status/1, fun stat_post/2]).

-spec version(protocol()) -> {ok,[non_neg_integer()],protocol()}.
version(#protocol{handle=H,version=V}=P)
  when undefined =/= H ->
    {ok, V, P}.

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
        S when ?ISSET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
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


%% == protected ==

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

-spec binary_to_integer(binary(),pos_integer(),non_neg_integer()) -> integer().
binary_to_integer(Binary, Base, _Decimals) ->
    binary_to_integer(Binary, Base).

%% -- protected: network --

-spec recv(protocol(),non_neg_integer()) -> {ok,binary(),protocol()}|{error,_,protocol()}.
recv(#protocol{handle=H,compress=Z}=P, Length) ->
    case myer_network:recv(H, Length, Z) of
        {ok, Binary, Handle} ->
            {ok, Binary, replace(P,Handle)};
        {error, Reason, Handle} ->
            {error, Reason, replace(P,Handle)}
    end.

-spec recv_packed_binary(protocol())
                        -> {ok,null|binary(),protocol()}|{error,_,protocol()}.
recv_packed_binary(Protocol) ->
    recv_packed_binary(Protocol, undefined).

-spec recv_packed_binary(protocol(),undefined|binary())
                        -> {ok,null|binary(),protocol()}|{error,_,protocol()}.
recv_packed_binary(#protocol{}=P, Byte) ->
    case recv_packed_integer(Byte, P) of
        {ok, null, Protocol} ->
            {ok, null, Protocol};
        {ok, 0, Protocol} ->
            {ok, <<>>, Protocol};
        {ok, Length, Protocol} ->
            recv(Protocol, Length)
    end.

%% == private ==

binary_to_version(Binary) ->
    F = fun(E) -> try binary_to_integer(E,10,0) catch _:_ -> E end end,
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
         ?CLIENT_PROTOCOL_41,
         ?CLIENT_TRANSACTIONS,
         ?CLIENT_SECURE_CONNECTION,
         ?CLIENT_MULTI_STATEMENTS,
         ?CLIENT_MULTI_RESULTS,
         ?CLIENT_PS_MULTI_RESULTS
        ],
    lists:foldl(fun(E,A) -> A bor E end, Caps, L).

func_recv_field(#protocol{caps=C})
  when ?ISSET(C,?CLIENT_PROTOCOL_41) ->
    fun myer_protocol_text:recv_field_41/2;
func_recv_field(_Protocol) ->
    fun myer_protocol_text:recv_field/2.

loop(Args, []) ->
    list_to_tuple([ok|Args]);
loop(Args, [H|T]) ->
    case apply(H, Args) of
        {ok, List} ->
            loop(List, T);
        {error, Reason} -> % connect_pre
            {error, Reason};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

merge(#protocol{caps=I}=P, #handshake{version=V,seed=S,caps=C,plugin=A}) ->
    P#protocol{version = V, seed = S, caps = (I band C), plugin = A}; % ignore: charset
merge(#protocol{}=P, #plugin{name=N}) ->
    P#protocol{plugin = N}.

replace(#protocol{}=P, Handle) ->
    P#protocol{handle = Handle}.

reset(#protocol{handle=H}=P) ->
    replace(P, myer_network:reset(H)).

zreset(#protocol{handle=H}=P) ->
    replace(P, myer_network:zreset(H)).

%% -- private: loop,auth --

auth_pre(User, Password, <<>>, #protocol{caps=C}=P) ->
    Protocol = P#protocol{caps = (C bxor ?CLIENT_CONNECT_WITH_DB)},
    {ok, [auth_to_binary(User,Password,<<>>,Protocol),Protocol]};
auth_pre(User, Password, Database, #protocol{caps=C}=P) ->
    Protocol = P#protocol{caps = (C bxor ?CLIENT_NO_SCHEMA)},
    {ok, [auth_to_binary(User,Password,Database,Protocol),Protocol]}.

auth_post(<<254>>, #protocol{}=P) ->
    recv_plugin(P);
auth_post(<<0>>, #protocol{caps=C}=P) ->
    recv_result(P#protocol{compress = ?ISSET(C,?CLIENT_COMPRESS)}).

auth_alt_pre(Password, #protocol{seed=S,plugin=A}=P) ->
    Scrambled = myer_auth:scramble(Password, S, A),
    {ok, [<<Scrambled/binary,0>>,P]}.

auth_alt_post(<<0>>, #protocol{caps=C}=P) ->
    recv_result(P#protocol{compress = ?ISSET(C,?CLIENT_COMPRESS)}).

%% -- private: loop,close --

close_pre(#protocol{}=P) ->
    {ok, [<<?COM_QUIT>>,reset(P)]}.

close_post(#protocol{handle=H}=P) ->
    _ = myer_network:close(H),
    {ok, [P#protocol{handle = undefined}]}.

%% -- private: loop,connect --

connect_pre(Address, Port, Charset, Compress, MaxLength, Timeout) ->
    case myer_network:connect(Address, Port, MaxLength, Timeout) of
        {ok, Handle} ->
            {ok, [#protocol{handle = Handle, maxlength = MaxLength, compress = false,
                            caps = default_caps(Compress), charset = Charset}]};
        {error, Reason} ->
            {error, Reason}
    end.

connect_post(<<10>>, #protocol{}=P) -> % "always 10"
    case recv(P, 0) of
        {ok, Binary, Protocol} ->
            {ok, [merge(Protocol,binary_to_handshake(Binary))]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- private: loop,next_result --

next_result_pre(#protocol{}=P) ->
    {ok, [zreset(P)]}.

%% -- priate: loop,ping --

ping_pre(#protocol{}=P) ->
    {ok, [<<?COM_PING>>,reset(P)]}.

%% -- private: loop,real_query --

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

%% -- priate: loop,refresh --

refresh_pre(Options, #protocol{}=P) ->
    {ok, [<<?COM_REFRESH,Options>>,reset(P)]}.

%% -- priate: loop,select_db --

select_db_pre(Database, #protocol{}=P) ->
    {ok, [<<?COM_INIT_DB,Database/binary>>,reset(P)]}.

%% -- priate: loop,stat --

stat_pre(#protocol{}=P) ->
    {ok, [<<?COM_STATISTICS>>,reset(P)]}.

stat_post(Byte, #protocol{}=P) ->
    case recv(P, 0) of
        {ok, Binary, Protocol} ->
            {ok, [iolist_to_binary([Byte,Binary]),Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- priate: loop,stmt_close --

stmt_close_pre(#prepare{stmt_id=S}=X, #protocol{}=P) ->
    {ok, [X,<<?COM_STMT_CLOSE,S:32/little>>,reset(P)]}.

stmt_close_post(_Prepare, #protocol{}=P) ->
    {ok, [P]}.

%% -- priate: loop,stmt_execute --

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
            stmt_execute_recv_rows(Result, X#prepare{field_count = N, fields = Fields}, Protocol);
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

stmt_execute_recv_rows(#result{status=S}=R, #prepare{execute=E}=X, #protocol{}=P)
  when ?ISSET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
    {ok, [X#prepare{result = R, execute = E+1},P]};
stmt_execute_recv_rows(_Result, #prepare{fields=F,execute=E}=X, #protocol{}=P) ->
    case recv_until_eof(fun myer_protocol_binary:recv_row/3, [F], [], P) of
        {ok, [Result,Rows,Protocol]} ->
	    {ok, [F,Rows,X#prepare{result = Result, execute = E+1},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- priate: loop,stmt_fetch --

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

%% -- private: loop,stmt_next_result --

stmt_next_result_pre(#prepare{}=X, #protocol{}=P) ->
    {ok, [X,zreset(P)]}.

stmt_next_result_post(<<0>>, #prepare{execute=E}=X, #protocol{}=P) ->
    case recv_result(P) of
        {ok, [Result,Protocol]} ->
            {ok, [X#prepare{result = Result, execute = E+1},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- private: loop,stmt_prepare --

stmt_prepare_pre(Query, #protocol{}=P) ->
    {ok, [<<?COM_STMT_PREPARE,Query/binary>>,reset(P)]}.

stmt_prepare_post(<<0>>, #protocol{version=V}=P) ->
    case recv(P, 0) of
	{ok, Binary, Protocol} ->
            stmt_prepare_recv_params(binary_to_prepare(V,Binary), Protocol);
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

%% -- private: loop,stmt_reset --

stmt_reset_pre(#prepare{stmt_id=S}=X, #protocol{}=P) ->
    {ok, [X,<<?COM_STMT_RESET,S:32/little>>,reset(P)]}.

stmt_reset_post(<<0>>, #prepare{}=X, #protocol{}=P) ->
    case recv_result(P) of
        {ok, [Result,Protocol]} ->
            {ok, [X#prepare{result = Result},Protocol]};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

%% -- private: network --

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

recv_plugin(#protocol{}=P) ->
    case recv(P, 0) of
	{ok, Binary, Protocol} ->
            {ok, [binary_to_plugin(Binary),Protocol]};
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

send(Binary, #protocol{handle=H,compress=Z}=P) ->
    case myer_network:send(H, Binary, Z) of
	{ok, Handle} ->
	    {ok, [replace(P,Handle)]};
	{error, Reason, Handle} ->
	    {error, Reason, replace(P,Handle)}
    end.

send(Term, Binary, #protocol{handle=H,compress=Z}=P) ->
    case myer_network:send(H, Binary, Z) of
	{ok, Handle} ->
	    {ok, [Term,replace(P,Handle)]};
	{error, Reason, Handle} ->
	    {error, Reason, replace(P,Handle)}
    end.

%% -- private: sql* --

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
            {binary_part(Binary,{S,Len}), S+Len, L-Len}
    end.

unpack_integer(_Binary, Start, 0) ->
    {0, Start, 0};
unpack_integer(Binary, Start, Length) ->
    case binary_part(Binary, {Start,1}) of
        <<254>> -> <<L:64/little>> = binary_part(Binary, {Start+1,8}), {L, Start+9, Length-9};
        <<253>> -> <<L:24/little>> = binary_part(Binary, {Start+1,4}), {L, Start+5, Length-5};
        <<252>> -> <<L:16/little>> = binary_part(Binary, {Start+1,2}), {L, Start+3, Length-3};
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
               #protocol{maxlength=M,seed=S,caps=C,charset=E,plugin=P})
  when ?ISSET(C,?CLIENT_PROTOCOL_41) ->
    X = myer_auth:scramble(Password, S, P),
    B = if ?ISSET(C,?CLIENT_SECURE_CONNECTION) -> N = size(X), <<N,X/binary>>;
           true                                -> <<X/binary,0>>
        end,
    D = if ?ISSET(C,?CLIENT_CONNECT_WITH_DB) -> <<Database/binary,0>>;
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
               #protocol{maxlength=M,seed=S,caps=C,plugin=P}) ->
    A = (C bor ?CLIENT_LONG_PASSWORD) band 16#ffff, % FORCE
    X = myer_auth:scramble(Password, S, P),
    B = if ?ISSET(C,?CLIENT_SECURE_CONNECTION) -> N = size(X), <<N,X/binary>>;
           true                                -> <<X/binary,0>>
        end,
    D = if ?ISSET(C,?CLIENT_CONNECT_WITH_DB) -> <<Database/binary,0>>;
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
  when ?ISSET(Caps,?CLIENT_PROTOCOL_41) ->
    <<W:16/little, S:16/little>> = Binary,
    #result{status = S, warning_count = W}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc : send_server_handshake_packet/3
%% -----------------------------------------------------------------------------
binary_to_handshake(Binary) ->
    binary_to_handshake(#handshake{}, Binary, 0, byte_size(Binary)).

binary_to_handshake(#handshake{version=undefined}=H, Binary, Start, Length) ->
    [B, _] = binary:split(binary_part(Binary,Start,Length), <<0>>), L = byte_size(B)+1,
    binary_to_handshake(H#handshake{version = binary_to_version(B)}, Binary, Start+L, Length-L);
binary_to_handshake(#handshake{tid=undefined}=H, Binary, Start, Length) ->
    <<N:32/little>> = binary_part(Binary, {Start,4}),
    binary_to_handshake(H#handshake{tid = N}, Binary, Start+4, Length-4);
binary_to_handshake(#handshake{seed=undefined}=H, Binary, Start, Length) ->
    [B, _] = binary:split(binary_part(Binary,{Start,Length}), <<0>>), L = byte_size(B)+1,
    binary_to_handshake(H#handshake{seed = B}, Binary, Start+L, Length-L);
binary_to_handshake(#handshake{caps=undefined}=H, Binary, Start, Length) ->
    <<I:16/little>> = binary_part(Binary, {Start,2}),
    binary_to_handshake(H#handshake{caps = I}, Binary, Start+2, Length-2);
binary_to_handshake(#handshake{version=V,seed=S1,caps=C1,charset=undefined}=H, Binary, Start, Length)
  when V >= [4,1,1]; ?ISSET(C1,?CLIENT_PROTOCOL_41) ->
    <<E>> = binary_part(Binary, Start, 1),
    <<I:16/little>> = binary_part(Binary, {Start+1,2}),
    <<C2:16/little>> = binary_part(Binary, {Start+3,2}),
    %% _X
    %% 0:10/integer-unit:8
    [S2, _] = binary:split(binary_part(Binary,{Start+16,Length-16}), <<0>>), L2 = byte_size(S2)+1,
    S = <<S1/binary, S2/binary>>,
    C = (C2 bsl 16) bor C1,
    binary_to_handshake(H#handshake{seed = S, caps = C, charset = E, status = I}, Binary, Start+16+L2, Length-16-L2);
binary_to_handshake(#handshake{charset=undefined}=H, Binary, Start, Length) ->
    <<E>> = binary_part(Binary, Start, 1),
    <<I:16/little>> = binary_part(Binary, Start+1, 2),
    %% 0:13/integer-unit:8
    binary_to_handshake(H#handshake{charset = E, status = I}, Binary, Start+16, Length-16);
binary_to_handshake(#handshake{version=V,caps=C,plugin=undefined}=H, Binary, Start, 0)
  when V >= [4,1,1]; ?ISSET(C,?CLIENT_PROTOCOL_41) ->
    binary_to_handshake(H#handshake{plugin = <<"mysql_native_password">>}, Binary, Start, 0);
binary_to_handshake(#handshake{plugin=undefined}=H, Binary, Start, 0) ->
    binary_to_handshake(H#handshake{plugin = <<>>}, Binary, Start, 0);
binary_to_handshake(#handshake{plugin=undefined}=H, Binary, Start, Length) ->
    [B, _] = binary:split(binary_part(Binary,Start,Length), <<0>>), L = byte_size(B)+1,
    binary_to_handshake(H#handshake{plugin = B}, Binary, Start+L, Length-L);
binary_to_handshake(Handshake, _Binary, _Start, 0) -> % byte_size(Binary) =:= Start
    Handshake.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc : send_plugin_request_packet/3
%% -----------------------------------------------------------------------------
binary_to_plugin(<<>>) ->
    #plugin{name = <<>>};
binary_to_plugin(Binary) ->
    [B, <<>>] = binary:split(Binary, <<0>>),
    #plugin{name = B}.

%% -----------------------------------------------------------------------------
%% << sql/sql_prepare.cc : send_prep_stmt/2
%% -----------------------------------------------------------------------------
binary_to_prepare(_Version, Binary) -> % < 5.0.0, warning_count=undefined
    <<S:32/little, F:16/little, P:16/little, 0, W:16/little>> = Binary,
    #prepare{stmt_id = S, field_count = F, param_count = P, warning_count = W,
             flags = ?CURSOR_TYPE_NO_CURSOR, prefetch_rows = 1, execute = 0}.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc : net_send_error_packet/4
%% -----------------------------------------------------------------------------
binary_to_reason(Caps, Binary, Start, Length)
  when ?ISSET(Caps,?CLIENT_PROTOCOL_41) ->
    <<E:16/little>> = binary_part(Binary, {Start,2}),
    <<$#>> = binary_part(Binary, {Start+2,1}),
    S = binary_part(Binary, {Start+3,5}),
    M = binary_part(Binary, {Start+8,Length-8}),
    #reason{errno = E, state = S, message = M};
binary_to_reason(_Caps, Binary, Start, Length) ->
    <<E:16/little>> = binary_part(Binary, {Start,2}),
    M = binary_part(Binary, {Start+2,Length-2}),
    #reason{errno = E, message = M}.

%% -----------------------------------------------------------------------------
%% << sql/protocol.cc : net_send_ok/6
%% -----------------------------------------------------------------------------
binary_to_result(Caps, Binary, Start, Length)
  when ?ISSET(Caps,?CLIENT_PROTOCOL_41) ->
    {A, S1, L1} = unpack_integer(Binary, Start, Length),
    {I, S2, L2} = unpack_integer(Binary, S1, L1),
    <<S:16/little, W:16/little>> = binary_part(Binary, {S2,4}),
    {M, _, 0} = unpack_binary(Binary, S2+4, L2-4),
    #result{affected_rows = A, insert_id = I, status = S, warning_count = W, message = M};
binary_to_result(_Caps, Binary, Start, Length) ->
    {N, S1, L1} = unpack_integer(Binary, Start, Length),
    {I, S2, L2} = unpack_integer(Binary, S1, L1),
    <<S:16/little>> = binary_part(Binary, {S2,2}), % < 4.0 -> S:8 ?
    {M, _, 0} = unpack_binary(Binary, S2+2, L2-2),
    #result{affected_rows = N, insert_id = I, status = S, message = M}.
