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

-module(myer_protocol_text_old).

-include("internal.hrl").

%% -- public --
-export([recv_field_41/3, recv_field/3]).
-export([recv_row/4]).

%% -- internal --
-import(myer_protocol, [binary_to_float/2,
                        recv_binary/3, recv_packed_binary/3]).

-type(caps() :: non_neg_integer()).

%% == public ==

%% @see sql/protocol.cc : Protocol_text::store*

-spec recv_field_41(handle(),caps(),byte()) -> {ok, fields(), handle()}.
recv_field_41(Handle, Caps, Byte) ->
    {ok, CT, H1} = recv_packed_binary(Byte, Caps, Handle),
    {ok, DB, H2} = recv_packed_binary(undefined, Caps, H1),
    {ok, TA, H3} = recv_packed_binary(undefined, Caps, H2),
    {ok, OT, H4} = recv_packed_binary(undefined, Caps, H3),
    {ok, NA, H5} = recv_packed_binary(undefined, Caps, H4),
    {ok, ON, H6} = recv_packed_binary(undefined, Caps, H5),
    {ok, B,  H7} = recv_binary(13, Caps, H6),
    <<12, E:16/little, L:32/little, T, F:16/little, N, 0, 0>> = B,
    {ok, #field{catalog = CT, db = DB, table = TA, org_table = OT,
                name = NA, org_name = ON, charsetnr = E, length = L,
                type = T, flags = F, decimals = N, cast = cast(T) }, H7}. % TODO: mask(flags)

-spec recv_field(handle(),caps(),byte()) -> {ok, fields(), handle()}.
recv_field(Handle, Caps, Byte) ->
    {ok, TA, H1} = recv_packed_binary(Byte, Caps, Handle),
    {ok, NA, H2} = recv_packed_binary(undefined, Caps, H1),
    {ok, B,  H3} = recv_binary(10, Caps, H2),
    <<3, L:24/little, 1, T, 3, F:16/little, N>> = B,
    {ok, #field{table = TA, name = NA, length = L,
                type = T, flags = F, decimals = N, cast = cast(T)}, H3}. % TODO: mask(flags)

-spec recv_row(handle(),caps(),binary(),fields()) -> {ok, rows(), handle()}.
recv_row(Handle, Caps, Byte, Fields) ->
    recv_row(Handle, Caps, Byte, Fields, []).

recv_row(Handle, _Caps, _Byte, [], List) ->
    {ok, lists:reverse(List), Handle};
recv_row(Handle, Caps, Byte, [#field{cast=C}=F|T], List) ->
    case recv_packed_binary(Byte, Caps, Handle) of
        {ok, null, H} ->
            recv_row(H, Caps, undefined, T, [null|List]);
        {ok, Binary, H} ->
            recv_row(H, Caps, undefined, T, [C(Binary,F)|List])
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
