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

-module(myer_protocol_text).

-include("myer_internal.hrl").

%% -- public --
-export([recv_field_41/2, recv_field/2]).
-export([recv_row/3]).

%% -- private --
-import(myer_protocol, [binary_to_float/2,
                        recv/2, recv_packed_binary/1, recv_packed_binary/2]).

%% == public ==

%% @see sql/protocol.cc : Protocol_text::store*

-spec recv_field_41(protocol(),binary()) -> {ok, field(), protocol()}.
recv_field_41(Protocol, Byte) ->
    {ok, CT, P1} = recv_packed_binary(Protocol, Byte),
    {ok, DB, P2} = recv_packed_binary(P1),
    {ok, TA, P3} = recv_packed_binary(P2),
    {ok, OT, P4} = recv_packed_binary(P3),
    {ok, NA, P5} = recv_packed_binary(P4),
    {ok, ON, P6} = recv_packed_binary(P5),
    {ok, B,  P7} = recv(P6, 13),
    <<12, E:16/little, L:32/little, T, F:16/little, N, 0, 0>> = B,
    {ok, #field{catalog = CT, db = DB, table = TA, org_table = OT,
                name = NA, org_name = ON, charsetnr = E, length = L,
                type = T, flags = F, decimals = N}, P7}. % TODO: mask(flags)

-spec recv_field(protocol(),binary()) -> {ok, field(), protocol()}.
recv_field(Protocol, Byte) ->
    {ok, TA, P1} = recv_packed_binary(Protocol, Byte),
    {ok, NA, P2} = recv_packed_binary(P1),
    {ok, B,  P3} = recv(P2, 10),
    <<3, L:24/little, 1, T, 3, F:16/little, N>> = B,
    {ok, #field{table = TA, name = NA, length = L,
                type = T, flags = F, decimals = N}, P3}. % TODO: mask(flags)

-spec recv_row(protocol(),binary(),[field()]) -> {ok, [term()], protocol()}.
recv_row(Protocol, Byte, Fields) ->
    recv_row(Protocol, Byte, Fields, []).

recv_row(Protocol, undefined, [], List) ->
    {ok, lists:reverse(List), Protocol};
recv_row(Protocol, Byte, [H|T], List) ->
    case restore(Protocol, Byte, type(H#field.type), H) of
        {ok, Value, #protocol{}=P} ->
            recv_row(P, undefined, T, [Value|List])
    end.

%% == private ==

cast(null, _Type, _Field) ->
    null;
cast(Binary, binary, _Field) ->
    Binary;
cast(Binary, integer, _Field) ->
    binary_to_integer(Binary);
cast(Binary, float, #field{decimals=D}) ->
    binary_to_float(Binary, D);
cast(Binary, datetime, _Field) ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day,Hour,Minute,Second], []} ->
            {{Year,Month,Day},{Hour,Minute,Second}};
        _ ->
            undefined % TODO: second_part
    end;
cast(Binary, date, _Field) ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day], []} ->
            {Year,Month,Day};
        _ ->
            undefined
    end;
cast(Binary, time, _Field) ->
    case io_lib:fread("~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Hour,Minute,Second], []} ->
            {Hour,Minute,Second};
        _ ->
            undefined % TODO: second_part
    end;
cast(Binary, bit, _Field) ->
    binary:decode_unsigned(Binary, big);
cast(_Binary, undefined, _Field) ->
    undefined.

restore(Protocol, Byte, Type, Field) ->
    case recv_packed_binary(Protocol, Byte) of
        {ok, Binary, #protocol{}=P} ->
            {ok, cast(Binary,Type,Field), P}
    end.

%%pe(?MYSQL_TYPE_DECIMAL)     -> undefined;
type(?MYSQL_TYPE_TINY)        -> integer;
type(?MYSQL_TYPE_SHORT)       -> integer;
type(?MYSQL_TYPE_LONG)        -> integer;
type(?MYSQL_TYPE_FLOAT)       -> float;
type(?MYSQL_TYPE_DOUBLE)      -> float;
%%pe(?MYSQL_TYPE_NULL)        -> undefined;
type(?MYSQL_TYPE_TIMESTAMP)   -> datetime;
type(?MYSQL_TYPE_LONGLONG)    -> integer;
type(?MYSQL_TYPE_INT24)       -> integer;
type(?MYSQL_TYPE_DATE)        -> date;
type(?MYSQL_TYPE_TIME)        -> time;
type(?MYSQL_TYPE_DATETIME)    -> datetime;
type(?MYSQL_TYPE_YEAR)        -> integer;
%%pe(?MYSQL_TYPE_NEWDATE)     -> undefined;
%%pe(?MYSQL_TYPE_VARCHAR)     -> undefined;
type(?MYSQL_TYPE_BIT)         -> bit;
%%pe(?MYSQL_TYPE_TIMESTAMP2)  -> undefined;
%%pe(?MYSQL_TYPE_DATETIME2)   -> undefined;
%%pe(?MYSQL_TYPE_TIME2)       -> undefined;
type(?MYSQL_TYPE_NEWDECIMAL)  -> float;
%%pe(?MYSQL_TYPE_ENUM)        -> undefined;
%%pe(?MYSQL_TYPE_SET)         -> undefined;
%%pe(?MYSQL_TYPE_TINY_BLOB)   -> undefined;
%%pe(?MYSQL_TYPE_MEDIUM_BLOB) -> undefined;
%%pe(?MYSQL_TYPE_LONG_BLOB)   -> undefined;
type(?MYSQL_TYPE_BLOB)        -> binary;
type(?MYSQL_TYPE_VAR_STRING)  -> binary;
type(?MYSQL_TYPE_STRING)      -> binary;
%%pe(?MYSQL_TYPE_GEOMETRY)    -> undefined;
type(_)                       -> undefined.
