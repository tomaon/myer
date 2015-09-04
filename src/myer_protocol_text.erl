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
-export([recv_field/2]).
-export([recv_row/3]).

%% -- internal --

-import(myer_handle, [recv_binary/2]).

-import(myer_protocol, [binary_to_float/2,
                        recv_packed_binary/1, recv_packed_binary/2, recv_unsigned/2]).

-type(handle() :: myer_handle:handle()).

%% == private ==

%% @see sql/protocol.cc : Protocol_text::store*

-spec recv_field(handle(),binary()) -> {ok,field(),handle()}.
recv_field(Handle, Byte) ->
    recv_field(#field{}, Byte, myer_handle:caps(Handle), Handle).

-spec recv_row(handle(),binary(),fields()) -> {ok,row(),handle()}.
recv_row(Handle, Byte, Fields) ->
    recv_row(Fields, Byte, Handle, []).

%% == internal ==

recv_field(#field{catalog=undefined}=F, Byte, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(Byte, Handle),
    recv_field(F#field{catalog=B}, Caps, S).

recv_field(#field{db=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(Handle),
    recv_field(F#field{db=B}, Caps, S);
recv_field(#field{table=undefined}=F, Caps, Handle) ->
    {ok, B, S} = recv_packed_binary(Handle),
    recv_field(F#field{table=B}, Caps, S);
recv_field(#field{org_table=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(Handle),
    recv_field(F#field{org_table=B}, Caps, S);
recv_field(#field{name=undefined}=F, Caps, Handle) ->
    {ok, B, S} = recv_packed_binary(Handle),
    recv_field(F#field{name=B}, Caps, S);
recv_field(#field{org_name=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(Handle),
    recv_field(F#field{org_name=B}, Caps, S);
recv_field(#field{reserved=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_binary(1, Handle), % <<12>>
    recv_field(F#field{reserved=B}, Caps, S);
recv_field(#field{reserved=undefined}=F, Caps, Handle) ->
    {ok, B, S} = recv_binary(1, Handle), % <<3>>
    recv_field(F#field{reserved=B}, Caps, S);
recv_field(#field{charsetnr=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(2, Handle),
    recv_field(F#field{charsetnr=U}, Caps, S);
recv_field(#field{length=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(4, Handle),
    recv_field(F#field{length=U}, Caps, S);
recv_field(#field{length=undefined}=F, Caps, Handle) ->
    {ok, U, S} = recv_unsigned(3, Handle),
    recv_field(F#field{length=U}, Caps, S);
recv_field(#field{reserved2=undefined}=F, Caps, Handle)
  when not(?IS_SET(Caps,?CLIENT_PROTOCOL_41)) ->
    {ok, B, S} = recv_binary(1, Handle), % <<1>>
    recv_field(F#field{reserved2=B}, Caps, S);
recv_field(#field{type=undefined}=F, Caps, Handle) ->
    {ok, U, S} = recv_unsigned(1, Handle),
    recv_field(F#field{type=U}, Caps, S);
recv_field(#field{reserved3=undefined}=F, Caps, Handle)
  when not(?IS_SET(Caps,?CLIENT_PROTOCOL_41)) ->
    {ok, B, S} = recv_binary(1, Handle), % <<3>>
    recv_field(F#field{reserved3=B}, Caps, S);
recv_field(#field{flags=undefined}=F, Caps, Handle) ->
    {ok, U, S} = recv_unsigned(2, Handle),
    recv_field(F#field{flags=U}, Caps, S);
recv_field(#field{decimals=undefined}=F, Caps, Handle) ->
    {ok, U, S} = recv_unsigned(1, Handle),
    recv_field(F#field{decimals=U}, Caps, S);
recv_field(#field{reserved4=undefined}=F, Caps, Handle)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_binary(2, Handle), % <<0,0>>
    recv_field(F#field{reserved4=B}, Caps, S);
recv_field(#field{type=T}=F, _Caps, Handle) ->
    {ok, F#field{cast = cast(T)}, Handle}.


recv_row2([], Handle, List) ->
    {ok, lists:reverse(List), Handle};
recv_row2([#field{cast=C}=H|T], S, List) ->
    case recv_packed_binary(S) of
        {ok, null, Handle} ->
            recv_row2(T, Handle, [null|List]);
        {ok, Binary, Handle} ->
            recv_row2(T, Handle, [C(Binary,H)|List])
    end.

recv_row([#field{cast=C}=H|T], Byte, S, List) ->
    case recv_packed_binary(Byte, S) of
        {ok, null, Handle} ->
            recv_row2(T, Handle, [null|List]);
        {ok, Binary, Handle} ->
            recv_row2(T, Handle, [C(Binary,H)|List])
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
