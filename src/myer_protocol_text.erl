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

%% -- internal --
-import(myer_protocol, [binary_to_float/2,
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
                type = T, flags = F, decimals = N, cast = cast(T) }, P7}. % TODO: mask(flags)

-spec recv_field(protocol(),binary()) -> {ok, field(), protocol()}.
recv_field(Protocol, Byte) ->
    {ok, TA, P1} = recv_packed_binary(Byte, Protocol),
    {ok, NA, P2} = recv_packed_binary(undefined, P1),
    {ok, B,  P3} = recv(P2, 10),
    <<3, L:24/little, 1, T, 3, F:16/little, N>> = B,
    {ok, #field{table = TA, name = NA, length = L,
                type = T, flags = F, decimals = N, cast = cast(T)}, P3}. % TODO: mask(flags)

-spec recv_row(protocol(),binary(),[field()]) -> {ok, [term()], protocol()}.
recv_row(Protocol, Byte, Fields) ->
    recv_row(Protocol, Byte, Fields, []).

recv_row(Protocol, _Byte, [], List) ->
    {ok, lists:reverse(List), Protocol};
recv_row(#protocol{}=P, Byte, [#field{cast=C}=H|T], List) ->
    case recv_packed_binary(Byte, P) of
        {ok, null, Protocol} ->
            recv_row(Protocol, undefined, T, [null|List]);
        {ok, Binary, Protocol} ->
            recv_row(Protocol, undefined, T, [C(Binary,H)|List])
    end.

%% == internal ==

%%st(?MYSQL_TYPE_DECIMAL)     -> undefined
cast(?MYSQL_TYPE_TINY)        -> fun to_integer/2;
cast(?MYSQL_TYPE_SHORT)       -> fun to_integer/2;
cast(?MYSQL_TYPE_LONG)        -> fun to_integer/2;
cast(?MYSQL_TYPE_FLOAT)       -> fun to_float/2;
cast(?MYSQL_TYPE_DOUBLE)      -> fun to_float/2;
%%st(?MYSQL_TYPE_NULL)        -> undefined
cast(?MYSQL_TYPE_TIMESTAMP)   -> fun to_datetime/2;
cast(?MYSQL_TYPE_LONGLONG)    -> fun to_integer/2;
cast(?MYSQL_TYPE_INT24)       -> fun to_integer/2;
cast(?MYSQL_TYPE_DATE)        -> fun to_date/2;
cast(?MYSQL_TYPE_TIME)        -> fun to_time/2;
cast(?MYSQL_TYPE_DATETIME)    -> fun to_datetime/2;
cast(?MYSQL_TYPE_YEAR)        -> fun to_integer/2;
%%st(?MYSQL_TYPE_NEWDATE)     -> undefined
%%st(?MYSQL_TYPE_VARCHAR)     -> undefined
cast(?MYSQL_TYPE_BIT)         -> fun to_bit/2;
%%st(?MYSQL_TYPE_TIMESTAMP2)  -> undefined
%%st(?MYSQL_TYPE_DATETIME2)   -> undefined
%%st(?MYSQL_TYPE_TIME2)       -> undefined
%%st(?MYSQL_TYPE_JSON)        -> undefined
cast(?MYSQL_TYPE_NEWDECIMAL)  -> fun to_float/2;
%%st(?MYSQL_TYPE_ENUM)        -> undefined
%%st(?MYSQL_TYPE_SET)         -> undefined
%%st(?MYSQL_TYPE_TINY_BLOB)   -> undefined
%%st(?MYSQL_TYPE_MEDIUM_BLOB) -> undefined
%%st(?MYSQL_TYPE_LONG_BLOB)   -> undefined
cast(?MYSQL_TYPE_BLOB)        -> fun to_binary/2;
cast(?MYSQL_TYPE_VAR_STRING)  -> fun to_binary/2;
cast(?MYSQL_TYPE_STRING)      -> fun to_binary/2.
%%st(?MYSQL_TYPE_GEOMETRY)    -> undefined;


to_binary(Binary, _Field) ->
    Binary.

to_bit(Binary, _Field) ->
    binary:decode_unsigned(Binary, big).

to_date(Binary, _Field) ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day], []} ->
            {Year,Month,Day}
    end.

to_datetime(Binary, _Field) ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day,Hour,Minute,Second], []} ->
            {{Year,Month,Day},{Hour,Minute,Second}}
            %%
            %% TODO: second_part
            %%
    end.

to_float(Binary, #field{decimals=D}) ->
    binary_to_float(Binary, D).

to_integer(Binary, _Field) ->
    binary_to_integer(Binary, 10).

to_time(Binary, _Field) ->
    case io_lib:fread("~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Hour,Minute,Second], []} ->
            {Hour,Minute,Second}
            %%
            %% TODO: second_part
            %%
    end.
