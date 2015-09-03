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
-export([recv_field/3]).
-export([recv_row/4]).

%% -- internal --
-import(myer_protocol, [binary_to_float/2,
                        recv/3, recv_packed_binary/3, recv_unsigned/3]).

-type(socket() :: tuple()). % TODO
-type(caps() :: non_neg_integer()).
-type(fields() :: [field()]).
-type(rows() :: [term()]).

%% == private ==

%% @see sql/protocol.cc : Protocol_text::store*

-spec recv_field(socket(),caps(),byte()) -> {ok, fields(), socket()}.
recv_field(Socket, Caps, Byte) ->
    recv_field(#field{}, Byte, Caps, Socket).

-spec recv_row(socket(),caps(),byte(),fields()) -> {ok, rows(), socket()}.
recv_row(Socket, Caps, Byte, Fields) ->
    recv_row(Fields, Byte, Caps, Socket, []).

%% == internal ==

recv_field(#field{catalog=undefined}=F, Byte, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(Byte, Caps, Socket),
    recv_field(F#field{catalog=B}, undefined, Caps, S);
recv_field(#field{db=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(undefined, Caps, Socket),
    recv_field(F#field{db=B}, undefined, Caps, S);
recv_field(#field{table=undefined}=F, undefined, Caps, Socket) ->
    {ok, B, S} = recv_packed_binary(undefined, Caps, Socket),
    recv_field(F#field{table=B}, undefined, Caps, S);
recv_field(#field{org_table=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(undefined, Caps, Socket),
    recv_field(F#field{org_table=B}, undefined, Caps, S);
recv_field(#field{name=undefined}=F, undefined, Caps, Socket) ->
    {ok, B, S} = recv_packed_binary(undefined, Caps, Socket),
    recv_field(F#field{name=B}, undefined, Caps, S);
recv_field(#field{org_name=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv_packed_binary(undefined, Caps, Socket),
    recv_field(F#field{org_name=B}, undefined, Caps, S);
recv_field(#field{reserved=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(1, Caps, Socket), % <<12>>
    recv_field(F#field{reserved=B}, undefined, Caps, S);
recv_field(#field{reserved=undefined}=F, undefined, Caps, Socket) ->
    {ok, B, S} = recv(1, Caps, Socket), % <<3>>
    recv_field(F#field{reserved=B}, undefined, Caps, S);
recv_field(#field{charsetnr=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_field(F#field{charsetnr=U}, undefined, Caps, S);
recv_field(#field{length=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, U, S} = recv_unsigned(4, Caps, Socket),
    recv_field(F#field{length=U}, undefined, Caps, S);
recv_field(#field{length=undefined}=F, undefined, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(3, Caps, Socket),
    recv_field(F#field{length=U}, undefined, Caps, S);
recv_field(#field{reserved2=undefined}=F, undefined, Caps, Socket)
  when not(?IS_SET(Caps,?CLIENT_PROTOCOL_41)) ->
    {ok, B, S} = recv(1, Caps, Socket), % <<1>>
    recv_field(F#field{reserved2=B}, undefined, Caps, S);
recv_field(#field{type=undefined}=F, undefined, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(1, Caps, Socket),
    recv_field(F#field{type=U}, undefined, Caps, S);
recv_field(#field{reserved3=undefined}=F, undefined, Caps, Socket)
  when not(?IS_SET(Caps,?CLIENT_PROTOCOL_41)) ->
    {ok, B, S} = recv(1, Caps, Socket), % <<3>>
    recv_field(F#field{reserved3=B}, undefined, Caps, S);
recv_field(#field{flags=undefined}=F, undefined, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(2, Caps, Socket),
    recv_field(F#field{flags=U}, undefined, Caps, S);
recv_field(#field{decimals=undefined}=F, undefined, Caps, Socket) ->
    {ok, U, S} = recv_unsigned(1, Caps, Socket),
    recv_field(F#field{decimals=U}, undefined, Caps, S);
recv_field(#field{reserved4=undefined}=F, undefined, Caps, Socket)
  when ?IS_SET(Caps,?CLIENT_PROTOCOL_41) ->
    {ok, B, S} = recv(2, Caps, Socket), % <<0,0>>
    recv_field(F#field{reserved4=B}, undefined, Caps, S);
recv_field(#field{type=T}=F, _Byte, _Caps, Socket) ->
    {ok, F#field{cast = cast(T)}, Socket}.


recv_row([], _Byte, _Caps, Socket, List) ->
    {ok, lists:reverse(List), Socket};
recv_row([#field{cast=C}=H|T], Byte, Caps, S, List) ->
    case recv_packed_binary(Byte, Caps, S) of
        {ok, null, Socket} ->
            recv_row(T, undefined, Caps, Socket, [null|List]);
        {ok, Binary, Socket} ->
            recv_row(T, undefined, Caps, Socket, [C(Binary,H)|List])
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
