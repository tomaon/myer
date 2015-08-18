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

-spec connect([term()]) -> {ok,undefined,protocol()}|{error,_,protocol()}.
connect(Args)
  when is_list(Args), 6 == length(Args) -> % TODO: proplists?
    L = [fun connect_pre/7, fun recv_status/1, fun connect_post/2],
    loop(#protocol{}, Args, L).

-spec close(protocol()) -> {ok,undefined,protocol()}|{error,_,protocol()}.
close(#protocol{handle=undefined}=P) ->
    {ok, undefined, P};
close(#protocol{}=P) ->
    L = [fun close_pre/1, fun send/2, fun close_post/1],
    loop(P, [], L).

-spec auth(protocol(),binary(),binary(),binary())
          -> {ok,result(),protocol()}|{error,_,protocol()}.
auth(#protocol{handle=H}=P, User, Password, Database)
  when undefined =/= H, is_binary(User), is_binary(Password), is_binary(Database) ->
    L = [fun auth_pre/4, fun send/2, fun recv_status/1, fun auth_post/2],
    case loop(P, [User,Password,Database], L) of
        {ok, #plugin{}=X, Protocol} ->
            auth_alt(merge(Protocol,X), Password);
        {ok, Result, Protocol}->
            {ok, Result, Protocol};
        {error, Reason, Protocol} ->
            {error, Reason, Protocol}
    end.

auth_alt(Protocol, Password) ->
    L = [fun auth_alt_pre/2, fun send/2, fun recv_status/1, fun auth_alt_post/2],
    loop(Protocol, [Password], L).

-spec ping(protocol()) -> {ok,result(),protocol()}|{error,_,protocol()}.
ping(#protocol{handle=H}=P)
  when undefined =/= H ->
    L = [fun ping_pre/1, fun send/2, fun recv_status/1, fun recv_result/2],
    loop(P, [], L).

-spec stat(protocol()) -> {ok,binary(),protocol()}|{error,_,protocol()}.
stat(#protocol{handle=H}=P)
  when undefined =/= H ->
    L = [fun stat_pre/1, fun send/2, fun recv_status/1, fun stat_post/2],
    loop(P, [], L).

-spec version(protocol()) -> {ok, [non_neg_integer()], protocol()}.
version(#protocol{version=V}=P) ->
    {ok, V, P}.

-spec real_query(protocol(),binary())
                -> {ok,result(),protocol()}|
                   {ok,{[field()],[term()],result()},protocol()}|{error,_,protocol()}.
real_query(#protocol{handle=H}=P, Query)
  when undefined =/= H, is_binary(Query) ->
    L = [fun real_query_pre/2, fun send/2, fun recv_status/1, fun real_query_post/2],
    loop(P, [Query], L).

-spec refresh(protocol(),integer()) -> {ok,result(),protocol()}|{error,_,protocol()}.
refresh(#protocol{handle=H}=P, Options)
  when undefined =/= H ->
    L = [fun refresh_pre/2, fun send/2, fun recv_status/1, fun recv_result/2],
    loop(P, [Options], L).

-spec select_db(protocol(),binary()) -> {ok,result(),protocol()}|{error,_,protocol()}.
select_db(#protocol{handle=H}=P, Database)
  when undefined =/= H, is_binary(Database) ->
    L = [fun select_db_pre/2, fun send/2, fun recv_status/1, fun recv_result/2],
    loop(P, [Database], L).

-spec stmt_prepare(protocol(),binary()) -> {ok,prepare(),protocol()}|{error,_,protocol()}.
stmt_prepare(#protocol{handle=H}=P, Query)
  when undefined =/= H, is_binary(Query) ->
    L = [fun stmt_prepare_pre/2, fun send/2, fun recv_status/1, fun stmt_prepare_post/2],
    loop(P, [Query], L).

-spec stmt_close(protocol(),prepare()) -> {ok,undefined,protocol()}|{error,_,protocol()}.
stmt_close(#protocol{handle=H}=P, #prepare{}=X)
  when undefined =/= H ->
    L = [fun stmt_close_pre/2, fun send/3, fun stmt_close_post/2],
    loop(P, [X], L).

-spec stmt_execute(protocol(),prepare(),[term()])
                  -> {ok,prepare(),protocol()}|
                     {ok,{[field()],[term()],prepare()},protocol()}|{error,_,protocol()}.
stmt_execute(#protocol{handle=H}=P, #prepare{param_count=N}=X, Args)
  when undefined =/= H, is_list(Args), N == length(Args) ->
    L = [fun stmt_execute_pre/3, fun send/3, fun recv_status/2, fun stmt_execute_post/3],
    loop(P, [X,Args], L).

-spec stmt_fetch(protocol(),prepare())
                -> {ok,prepare(),protocol()}|
                   {ok,{[field()],[term()],prepare()},protocol()}|{error,_,protocol()}.
stmt_fetch(#protocol{handle=H}=P, #prepare{result=R}=X)
  when undefined =/= H ->
    case R#result.status of
        S when ?ISSET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
            L = [fun stmt_fetch_pre/2, fun send/3, fun stmt_fetch_post/2],
            loop(P, [X], L);
        _ ->
            {ok, X, P}
    end.

-spec stmt_reset(protocol(),prepare()) -> {ok,prepare(),protocol()}|{error,_,protocol()}.
stmt_reset(#protocol{handle=H}=P, #prepare{}=X)
  when undefined =/= H ->
    L = [fun stmt_reset_pre/2, fun send/3, fun recv_status/2, fun stmt_reset_post/3],
    loop(P, [X], L).

-spec next_result(protocol())
                 -> {ok,result(),protocol()}|
                    {ok,{[field()],[term()],result()},protocol()}|{error,_,protocol()}.
next_result(#protocol{handle=H}=P)
  when undefined =/= H ->
    L = [fun next_result_pre/1, fun recv_status/1, fun real_query_post/2],
    loop(P, [], L).

stmt_next_result(#protocol{handle=H}=P, #prepare{}=X)
  when undefined =/= H ->
    L = [fun stmt_next_result_pre/2, fun recv_status/2, fun stmt_next_result_post/3],
    loop(P, [X], L).

%% == protected ==

-spec binary_to_float(binary(),non_neg_integer()) -> float().
binary_to_float(Binary, _Decimals) ->
    L = binary_to_list(Binary),
    try list_to_float(L) % erlang:binary_to_float/1, > R16
    catch
        _:_ ->
            try list_to_integer(L) of % erlang:binary_to_integer/2, > R16
                I ->
                    I * 1.0
            catch
                _:_ ->
                    undefined % for v5.1 (out_of_range)
            end
    end.

-spec binary_to_integer(binary(),pos_integer(),non_neg_integer()) -> integer().
binary_to_integer(Binary, _Base, _Decimals) ->
    %%erlang:binary_to_integer/2, > R16
    L = binary_to_list(Binary),
    list_to_integer(L).

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
    case recv_packed_integer(P, Byte) of
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

loop(Protocol, Term, []) ->
    {ok, Term, Protocol};
loop(#protocol{}=P, Term, [H|T])
  when is_list(Term) ->
    case apply(H, [P|Term]) of
        {ok, List, Protocol} ->
            loop(Protocol, List, T);
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

auth_pre(#protocol{caps=C}=P, User, Password, <<>>) ->
    Protocol = P#protocol{caps = (C bxor ?CLIENT_CONNECT_WITH_DB)},
    {ok, [auth_to_binary(Protocol,User,Password,<<>>)], Protocol};
auth_pre(#protocol{caps=C}=P, User, Password, Database) ->
    Protocol = P#protocol{caps = (C bxor ?CLIENT_NO_SCHEMA)},
    {ok, [auth_to_binary(Protocol,User,Password,Database)], Protocol}.

auth_post(Protocol, <<254>>) ->
    recv_plugin(Protocol);
auth_post(#protocol{caps=C}=P, <<0>>) ->
    recv_result(P#protocol{compress = ?ISSET(C,?CLIENT_COMPRESS)}).

auth_alt_pre(#protocol{seed=S,plugin=A}=P, Password) ->
    Scrambled = myer_auth:scramble(Password, S, A),
    {ok, [<<Scrambled/binary,0>>], P}.

auth_alt_post(#protocol{caps=C}=P, <<0>>) ->
    recv_result(P#protocol{compress = ?ISSET(C,?CLIENT_COMPRESS)}).

%% -- private: loop,close --

close_pre(Protocol) ->
    {ok, [<<?COM_QUIT>>], reset(Protocol)}.

close_post(#protocol{handle=H}=P) ->
    _ = myer_network:close(H),
    {ok, undefined, P#protocol{handle = undefined}}.

%% -- private: loop,connect --

connect_pre(#protocol{}=P, Address, Port, Charset, Compress, MaxLength, Timeout) ->
    case myer_network:connect(Address, Port, MaxLength, Timeout) of
        {ok, Handle} ->
            {ok, [], P#protocol{handle = Handle, maxlength = MaxLength, compress = false,
                                caps = default_caps(Compress), charset = Charset}};
        {error, Reason} ->
            {error, Reason, P}
    end.

connect_post(Protocol, <<10>>) -> % "always 10"
    case recv_rest(Protocol) of
        {ok, Binary, #protocol{}=P} ->
            {ok, undefined, merge(P,binary_to_handshake(Binary))};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- private: loop,next_result --

next_result_pre(Protocol) ->
    {ok, [], zreset(Protocol)}.

%% -- priate: loop,ping --

ping_pre(Protocol) ->
    {ok, [<<?COM_PING>>], reset(Protocol)}.

%% -- private: loop,real_query --

real_query_pre(Protocol, Query) ->
    {ok, [<<?COM_QUERY,Query/binary>>], reset(Protocol)}.

real_query_post(Protocol, <<0>>) ->
    recv_result(Protocol);
real_query_post(Protocol, Byte) ->
    case recv_packed_integer(Protocol, Byte) of
        {ok, N, #protocol{}=P} ->
            real_query_recv_fields(P, N);
        {error, Reason, P} ->
            {error, Reason, P}
    end.

real_query_recv_fields(Protocol, N) ->
    case recv_until_eof(Protocol, func_recv_field(Protocol), [], []) of
        {ok, _Result, Fields, #protocol{}=P} when N == length(Fields) ->
            real_query_recv_rows(P, Fields);
        {error, Reason, P} ->
            {error, Reason, P}
    end.

real_query_recv_rows(Protocol, Fields) ->
    case recv_until_eof(Protocol, fun myer_protocol_text:recv_row/3, [Fields], []) of
        {ok, Result, Rows, #protocol{}=P} ->
	    {ok, {Fields,Rows,Result}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- priate: loop,refresh --

refresh_pre(Protocol, Options) ->
    {ok, [<<?COM_REFRESH,Options>>], reset(Protocol)}.

%% -- priate: loop,select_db --

select_db_pre(Protocol, Database) ->
    {ok, [<<?COM_INIT_DB,Database/binary>>], reset(Protocol)}.

%% -- priate: loop,stat --

stat_pre(Protocol) ->
    {ok, [<<?COM_STATISTICS>>], reset(Protocol)}.

stat_post(Protocol, Byte) ->
    case recv_rest(Protocol) of
        {ok, Binary, #protocol{}=P} ->
            {ok, iolist_to_binary([Byte,Binary]), P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- priate: loop,stmt_close --

stmt_close_pre(Protocol, #prepare{stmt_id=S}=X) ->
    {ok, [X,<<?COM_STMT_CLOSE,S:32/little>>], reset(Protocol)}.

stmt_close_post(Protocol, _Prepare) ->
    {ok, undefined, Protocol}.

%% -- priate: loop,stmt_execute --

stmt_execute_pre(Protocol, Prepare, Args) ->
    B = stmt_execute_to_binary(Prepare, Args),
    {ok, [Prepare,B], reset(Protocol)}.

stmt_execute_post(Protocol, #prepare{execute=E}=X, <<0>>) ->
    case recv_result(Protocol) of
        {ok, Result, #protocol{}=P} ->
            {ok, X#prepare{result = Result, execute = E+1}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end;
stmt_execute_post(Protocol, Prepare, Byte) ->
    case recv_packed_integer(Protocol, Byte) of
        {ok, N, #protocol{}=P} ->
            stmt_execute_recv_fields(P, Prepare, N);
        {error, Reason, P} ->
            {error, Reason, P}
    end.

stmt_execute_recv_fields(Protocol, #prepare{}=X, N) -> % do CALL, 0=X.filed_count
    case recv_until_eof(Protocol, func_recv_field(Protocol), [], []) of
        {ok, Result, Fields, #protocol{}=P} when N == length(Fields) ->
            stmt_execute_recv_rows(P, Result, X#prepare{field_count = N, fields = Fields});
        {error, Reason, P} ->
            {error, Reason, P}
    end.

stmt_execute_recv_rows(Protocol, #result{status=S}=R, #prepare{execute=E}=X)
  when ?ISSET(S,?SERVER_STATUS_CURSOR_EXISTS) ->
    {ok, X#prepare{result = R, execute = E+1}, Protocol};
stmt_execute_recv_rows(Protocol, _Result, #prepare{fields=F,execute=E}=X) ->
    case recv_until_eof(Protocol, fun myer_protocol_binary:recv_row/3, [F], []) of
        {ok, Result, Rows, #protocol{}=P} ->
	    {ok, {F,Rows,X#prepare{result = Result, execute = E+1}}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- priate: loop,stmt_fetch --

stmt_fetch_pre(Protocol, #prepare{stmt_id=S,prefetch_rows=R}=X) ->
    B = <<?COM_STMT_FETCH, S:32/little, R:32/little>>,
    {ok, [X,B], reset(Protocol)}.

stmt_fetch_post(Protocol, #prepare{fields=F}=X) ->
    case recv_until_eof(Protocol, fun myer_protocol_binary:recv_row/3, [F], []) of
        {ok, Result, Rows, #protocol{}=P} ->
	    {ok, {F,Rows,X#prepare{result = Result}}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- private: loop,stmt_next_result --

stmt_next_result_pre(Protocol, #prepare{}=X) ->
    {ok, [X], zreset(Protocol)}.

stmt_next_result_post(Protocol, #prepare{execute=E}=X, <<0>>) ->
    case recv_result(Protocol) of
        {ok, Result, #protocol{}=P} ->
            {ok, X#prepare{result = Result, execute = E+1}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- private: loop,stmt_prepare --

stmt_prepare_pre(Protocol, Query) ->
    {ok, [<<?COM_STMT_PREPARE,Query/binary>>], reset(Protocol)}.

stmt_prepare_post(Protocol, <<0>>) ->
    case recv_rest(Protocol) of
	{ok, Binary, #protocol{version=V}=P} ->
            stmt_prepare_recv_params(P, binary_to_prepare(V,Binary));
        {error, Reason, P} ->
            {error, Reason, P}
    end.

stmt_prepare_recv_params(Protocol, #prepare{param_count=0}=X) ->
    stmt_prepare_recv_fields(Protocol, undefined, X#prepare{params = []});
stmt_prepare_recv_params(Protocol, #prepare{param_count=N}=X) ->
    case recv_until_eof(Protocol, func_recv_field(Protocol), [], []) of
        {ok, Result, Params, #protocol{}=P} when N == length(Params)->
            stmt_prepare_recv_fields(P, Result, X#prepare{params = Params});
        {error, Reason, P} ->
            {error, Reason, P}
    end.

stmt_prepare_recv_fields(Protocol, Result, #prepare{field_count=0}=X) ->
    {ok, X#prepare{result = Result}, Protocol};
stmt_prepare_recv_fields(Protocol, _Result, #prepare{field_count=N}=X) ->
    case recv_until_eof(Protocol, func_recv_field(Protocol), [], []) of
        {ok, Result, Fields, #protocol{}=P} when N == length(Fields) ->
            {ok, X#prepare{fields = Fields, result = Result}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- private: loop,stmt_reset --

stmt_reset_pre(Protocol, #prepare{stmt_id=S}=X) ->
    {ok, [X,<<?COM_STMT_RESET,S:32/little>>], reset(Protocol)}.

stmt_reset_post(Protocol, #prepare{}=X, <<0>>) ->
    case recv_result(Protocol) of
        {ok, Result, #protocol{}=P} ->
            {ok, X#prepare{result = Result}, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

%% -- private: network --

recv_eof(Protocol, Term) ->
    case recv_rest(Protocol) of
	{ok, Binary, #protocol{caps=C}=P} ->
            {ok, binary_to_eof(C,Binary), Term, P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_error(Protocol) ->
    case recv_rest(Protocol) of
	{ok, Binary, #protocol{caps=C}=P} ->
            {error, binary_to_reason(C,Binary,0,byte_size(Binary)), P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_plugin(Protocol) ->
    case recv_rest(Protocol) of
	{ok, Binary, #protocol{}=P} ->
            {ok, binary_to_plugin(Binary), P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_rest(Protocol) ->
    recv(Protocol, 0).

recv_result(Protocol) ->
    case recv_rest(Protocol) of
	{ok, Binary, #protocol{caps=C}=P} ->
            {ok, binary_to_result(C,Binary,0,byte_size(Binary)), P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_result(Protocol, <<0>>) ->
    recv_result(Protocol).

recv_status(Protocol) ->
    case recv(Protocol, 1) of
	{ok, <<255>>, #protocol{}=P} ->
            recv_error(P);
        {ok, Byte, #protocol{}=P} ->
            {ok, [Byte], P}; % forward
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_status(Protocol, Term) ->
    case recv(Protocol, 1) of
	{ok, <<255>>, #protocol{}=P} ->
            recv_error(P);
        {ok, Byte, #protocol{}=P} ->
            {ok, [Term,Byte], P}; % forward
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_unsigned(Protocol, Length) ->
    case recv(Protocol, Length) of
        {ok, Binary, #protocol{}=P} ->
            {ok, binary:decode_unsigned(Binary,little), P};
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_until_eof(Protocol, Func, Args, List) ->
    case recv(Protocol, 1) of
        {ok, <<255>>, #protocol{}=P} ->
            recv_error(P);
	{ok, <<254>>, #protocol{}=P} ->
            recv_eof(P, lists:reverse(List));
        {ok, Byte, #protocol{}=P} ->
            recv_until_eof(P, Func, Args, List, Byte);
        {error, Reason, P} ->
            {error, Reason, P}
    end.

recv_until_eof(Protocol, Func, Args, List, Byte) ->
    case apply(Func, [Protocol|[Byte|Args]]) of
        {ok, Term, #protocol{}=P} ->
            recv_until_eof(P, Func, Args, [Term|List]);
        {error, Reason, P} ->
            {error, Reason, P}
    end.

send(#protocol{handle=H,compress=Z}=P, Binary) ->
    case myer_network:send(H, Binary, Z) of
	{ok, Handle} ->
	    {ok, [], replace(P,Handle)};
	{error, Reason, Handle} ->
	    {error, Reason, replace(P,Handle)}
    end.

send(#protocol{handle=H,compress=Z}=P, Term, Binary) ->
    case myer_network:send(H, Binary, Z) of
	{ok, Handle} ->
	    {ok, [Term], replace(P,Handle)};
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

recv_packed_integer(Protocol, undefined) ->
    case recv(Protocol, 1) of
        {ok, Byte, #protocol{}=P} ->
            recv_packed_integer(P, Byte);
        {error, Reason, P} ->
            {error, Reason, P}
    end;
recv_packed_integer(Protocol, <<254>>) -> recv_unsigned(Protocol, 8);
recv_packed_integer(Protocol, <<253>>) -> recv_unsigned(Protocol, 3);
recv_packed_integer(Protocol, <<252>>) -> recv_unsigned(Protocol, 2);
recv_packed_integer(Protocol, <<251>>) -> {ok, null, Protocol};
recv_packed_integer(Protocol, <<Int>>) -> {ok, Int, Protocol}.

%% -----------------------------------------------------------------------------
%% << sql-common/client.c : send_client_reply_packet/3
%% -----------------------------------------------------------------------------
auth_to_binary(#protocol{maxlength=M,seed=S,caps=C,charset=E,plugin=P},
               User, Password, Database)
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
auth_to_binary(#protocol{maxlength=M,seed=S,caps=C,plugin=P},
               User, Password, Database) ->
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
