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

%% -- private --
-export([recv_field_41/2, recv_field/2]).
-export([recv_row/3]).

%% -- internal --
-import(myer_handle, [recv_binary/2]).
-import(myer_protocol, [recv_packed_binary/1, recv_packed_binary/2]).

-type(handle() :: myer_handle:handle()).

%% == private ==

%% @see sql/protocol.cc : Protocol_text::store*

-spec recv_field_41(handle(),binary()) -> {ok,field(),handle()}.
recv_field_41(Handle, Byte) ->
    {ok, CT, H1} = recv_packed_binary(Byte, Handle),
    {ok, DB, H2} = recv_packed_binary(H1),
    {ok, TA, H3} = recv_packed_binary(H2),
    {ok, OT, H4} = recv_packed_binary(H3),
    {ok, NA, H5} = recv_packed_binary(H4),
    {ok, ON, H6} = recv_packed_binary(H5),
    {ok, B,  H7} = recv_binary(13, H6),
    <<12, E:16/little, L:32/little, T, F:16/little, N, 0, 0>> = B,
    {ok, #field{catalog = CT, db = DB, table = TA, org_table = OT,
                name = NA, org_name = ON, charsetnr = E, length = L,
                type = T, flags = F, decimals = N}, H7}. % TODO: mask(flags)

-spec recv_field(handle(),binary()) -> {ok,field(),handle()}.
recv_field(Handle, Byte) ->
    {ok, TA, H1} = recv_packed_binary(Byte, Handle),
    {ok, NA, H2} = recv_packed_binary(H1),
    {ok, B,  H3} = recv_binary(10, H2),
    <<3, L:24/little, 1, T, 3, F:16/little, N>> = B,
    {ok, #field{table = TA, name = NA, length = L,
                type = T, flags = F, decimals = N}, H3}. % TODO: mask(flags)

-spec recv_row(handle(),binary(),fields()) -> {ok,row(),handle()}.
recv_row(Handle, Byte, Fields) ->
    recv_columns(Handle, Byte, [ decode(T) || #field{type=T} <- Fields], []).


recv_columns(Handle, [], List) ->
    {ok, lists:reverse(List), Handle};
recv_columns(Handle, [C|T], List) ->
    case recv_packed_binary(Handle) of
        {ok, null, H} ->
            recv_columns(H, T, [null|List]);
        {ok, Binary, H} ->
            recv_columns(H, T, [C(Binary)|List])
    end.

recv_columns(Handle, Byte, [C|T], List) ->
    case recv_packed_binary(Byte, Handle) of
        {ok, null, H} ->
            recv_columns(H, T, [null|List]);
        {ok, Binary, H} ->
            recv_columns(H, T, [C(Binary)|List])
    end.

%% == internal ==

%%code(?MYSQL_TYPE_DECIMAL)     -> undefined
decode(?MYSQL_TYPE_TINY)        -> fun to_integer/1;
decode(?MYSQL_TYPE_SHORT)       -> fun to_integer/1;
decode(?MYSQL_TYPE_LONG)        -> fun to_integer/1;
decode(?MYSQL_TYPE_FLOAT)       -> fun to_float/1;
decode(?MYSQL_TYPE_DOUBLE)      -> fun to_float/1;
%%code(?MYSQL_TYPE_NULL)        -> undefined
decode(?MYSQL_TYPE_TIMESTAMP)   -> fun to_datetime/1;
decode(?MYSQL_TYPE_LONGLONG)    -> fun to_integer/1;
decode(?MYSQL_TYPE_INT24)       -> fun to_integer/1;
decode(?MYSQL_TYPE_DATE)        -> fun to_date/1;
decode(?MYSQL_TYPE_TIME)        -> fun to_time/1;
decode(?MYSQL_TYPE_DATETIME)    -> fun to_datetime/1;
decode(?MYSQL_TYPE_YEAR)        -> fun to_integer/1;
%%code(?MYSQL_TYPE_NEWDATE)     -> undefined
%%code(?MYSQL_TYPE_VARCHAR)     -> undefined
decode(?MYSQL_TYPE_BIT)         -> fun to_bit/1;
%%code(?MYSQL_TYPE_TIMESTAMP2)  -> undefined
%%code(?MYSQL_TYPE_DATETIME2)   -> undefined
%%code(?MYSQL_TYPE_TIME2)       -> undefined
%%code(?MYSQL_TYPE_JSON)        -> undefined
decode(?MYSQL_TYPE_NEWDECIMAL)  -> fun to_float/1;
%%code(?MYSQL_TYPE_ENUM)        -> undefined
%%code(?MYSQL_TYPE_SET)         -> undefined
%%code(?MYSQL_TYPE_TINY_BLOB)   -> undefined
%%code(?MYSQL_TYPE_MEDIUM_BLOB) -> undefined
%%code(?MYSQL_TYPE_LONG_BLOB)   -> undefined
decode(?MYSQL_TYPE_BLOB)        -> fun to_binary/1;
decode(?MYSQL_TYPE_VAR_STRING)  -> fun to_binary/1;
decode(?MYSQL_TYPE_STRING)      -> fun to_binary/1.
%%code(?MYSQL_TYPE_GEOMETRY)    -> undefined;


to_binary(Binary) ->
    Binary.

to_bit(Binary) ->
    binary:decode_unsigned(Binary, big).

to_date(Binary) ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day], []} ->
            {Year,Month,Day}
    end.

to_datetime(Binary) ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Year,Month,Day,Hour,Minute,Second], []} ->
            {{Year,Month,Day},{Hour,Minute,Second}}
            %%
            %% TODO: second_part
            %%
    end.

to_float(Binary) ->
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

to_integer(Binary) ->
    binary_to_integer(Binary, 10).

to_time(Binary) ->
    case io_lib:fread("~d:~d:~d", binary_to_list(Binary)) of
        {ok, [Hour,Minute,Second], []} ->
            {Hour,Minute,Second}
            %%
            %% TODO: second_part
            %%
    end.
