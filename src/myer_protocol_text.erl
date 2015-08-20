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

-module(myer_protocol_text).

-include("internal.hrl").

%% -- public --
-export([recv_field_41/2, recv_field/2]).
-export([recv_row/3]).

%% -- private --
-import(myer_protocol, [binary_to_float/2, binary_to_integer/3,
                        recv/2, recv_packed_binary/2]).

%% == public ==

%% @see sql/protocol.cc : Protocol_text::store*

-spec recv_field_41(protocol(),binary()) -> {ok, field(), protocol()}.
recv_field_41(Protocol, Byte) ->
    {ok, CT, P1} = recv_packed_binary(Byte, Protocol),
    {ok, DB, P2} = recv_packed_binary(undefined, P1),
    {ok, TA, P3} = recv_packed_binary(undefined, P2),
    {ok, OT, P4} = recv_packed_binary(undefined, P3),
    {ok, NA, P5} = recv_packed_binary(undefined, P4),
    {ok, ON, P6} = recv_packed_binary(undefined, P5),
    {ok, B,  P7} = recv(P6, 13),
    <<12, E:16/little, L:32/little, T, F:16/little, N, 0, 0>> = B,
    {ok, #field{catalog = CT, db = DB, table = TA, org_table = OT,
                name = NA, org_name = ON, charsetnr = E, length = L,
                type = T, flags = F, decimals = N, cast = type(T) }, P7}. % TODO: mask(flags)

-spec recv_field(protocol(),binary()) -> {ok, field(), protocol()}.
recv_field(Protocol, Byte) ->
    {ok, TA, P1} = recv_packed_binary(Byte, Protocol),
    {ok, NA, P2} = recv_packed_binary(undefined, P1),
    {ok, B,  P3} = recv(P2, 10),
    <<3, L:24/little, 1, T, 3, F:16/little, N>> = B,
    {ok, #field{table = TA, name = NA, length = L,
                type = T, flags = F, decimals = N, cast = type(T)}, P3}. % TODO: mask(flags)

-spec recv_row(protocol(),binary(),[field()]) -> {ok, [term()], protocol()}.
recv_row(Protocol, Byte, Fields) ->
    recv_row(Protocol, Byte, Fields, []).

recv_row(Protocol, undefined, [], List) ->
    {ok, lists:reverse(List), Protocol};
recv_row(Protocol, Byte, [H|T], List) ->
    case recv_packed_binary(Byte, Protocol) of
        {ok, Binary, #protocol{}=P} ->
            recv_row(P, undefined, T, [cast(Binary,H)|List])
    end.

%% == private ==

cast(null, _Field) ->
    null;
cast(Binary, #field{cast={integer,_},decimals=D}) ->
    binary_to_integer(Binary, 10, D);
cast(Binary, #field{cast={float,_},decimals=D}) ->
    binary_to_float(Binary, D);
cast(Binary, #field{cast=binary}) ->
    Binary;
cast(Binary, #field{cast=datetime}) ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day,Hour,Minute,Second], []} ->
            {{Year,Month,Day},{Hour,Minute,Second}};
        _ ->
            undefined % TODO: second_part
    end;
cast(Binary, #field{cast=date}) ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day], []} ->
            {Year,Month,Day};
        _ ->
            undefined
    end;
cast(Binary, #field{cast=time}) ->
    case io_lib:fread("~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Hour,Minute,Second], []} ->
            {Hour,Minute,Second};
        _ ->
            undefined % TODO: second_part
    end;
cast(Binary, #field{cast=decimal,decimals=D}) ->
    binary_to_float(Binary, D);
cast(Binary, #field{cast=bit}) ->
    binary:decode_unsigned(Binary, big);
cast(_Binary, _Field) ->
    undefined.

%%pe(?MYSQL_TYPE_DECIMAL)     -> undefined;
type(?MYSQL_TYPE_TINY)        -> {integer,1};
type(?MYSQL_TYPE_SHORT)       -> {integer,2};
type(?MYSQL_TYPE_LONG)        -> {integer,4};
type(?MYSQL_TYPE_FLOAT)       -> {float,4};
type(?MYSQL_TYPE_DOUBLE)      -> {float,8};
%%pe(?MYSQL_TYPE_NULL)        -> undefined;
type(?MYSQL_TYPE_TIMESTAMP)   -> datetime;
type(?MYSQL_TYPE_LONGLONG)    -> {integer,8};
type(?MYSQL_TYPE_INT24)       -> {integer,4};
type(?MYSQL_TYPE_DATE)        -> date;
type(?MYSQL_TYPE_TIME)        -> time;
type(?MYSQL_TYPE_DATETIME)    -> datetime;
type(?MYSQL_TYPE_YEAR)        -> {integer,2};
%%pe(?MYSQL_TYPE_NEWDATE)     -> undefined;
%%pe(?MYSQL_TYPE_VARCHAR)     -> undefined;
type(?MYSQL_TYPE_BIT)         -> bit;
%%pe(?MYSQL_TYPE_TIMESTAMP2)  -> undefined;
%%pe(?MYSQL_TYPE_DATETIME2)   -> undefined;
%%pe(?MYSQL_TYPE_TIME2)       -> undefined;
type(?MYSQL_TYPE_NEWDECIMAL)  -> decimal;
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
