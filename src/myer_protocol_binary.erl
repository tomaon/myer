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

-module(myer_protocol_binary).

-include("internal.hrl").

%% -- public --
-export([prepare_fields/1]).
-export([recv_row/3]).

%% -- internal --
-import(myer_protocol, [binary_to_float/2,
                        recv_binary/2, recv_packed_binary/2]).

%% @see sql/protocol.cc : Protocol_binary::store*
%%
%% +---+------+------+---------------------+
%% | k | v    | d    | t                   | k:integer, v:varchar, d:decimal, t:timestamp
%% +---+------+------+---------------------+
%% | 1 | aaa  | NULL | NULL                |
%% | 2 | NULL | NULL | NULL                |
%% | 3 |      | NULL | NULL                |
%% | 4 | NULL | 3.14 | NULL                |
%% | 5 | NULL | NULL | 2012-01-30 16:36:00 |
%% +---+------+------+---------------------+
%% uint offset= (field_pos+2)/8+1, bit= (1 << ((field_pos+2) & 7));
%%            k={ (0+2)/8+1=1, 1<<2=(0+2)&7=  4=0000 0100 }
%%            v={ (1+2)/8+1=1, 1<<3=(1+2)&7=  8=0000 1000 }
%%            d={ (2+2)/8+1=1, 1<<4=(2+2)&7= 16=0001 0000 }
%%            t={ (3+2)/8+1=1, 1<<5=(3+2)&7= 32=0010 0000 }
%% field_count=4, null_field=(4+7+2)/8=1        td vk
%% 1=<<0,48,1,0,0,0,3,97,97,97>>           0, 0011-0000, 1,0,0,0, 3,97,97,97
%% 2=<<0,56,2,0,0,0>>                      0, 0011-1000, 2,0,0,0
%% 3=<<0,48,3,0,0,0,0>>                    0, 0011-0000, 3,0,0,0, 0
%% 4=<<0,40,4,0,0,0,4,51,46,49,52>>        0, 0010-1000, 4,0,0,0, 4,51,46,49,52
%% 5=<<0,24,5,0,0,0,7,220,7,1,30,16,36,0>> 0, 0001-1000, 5,0,0,0, 7,220,7,1,30,16,36,0,
%%                                                              len   Y=2 M  D  H, M S,second_part=4 (7->11)
%%
%% +---+------+------+---------------------+------+------+------+
%% | k | v    | d    | t                   | a    | b    | c    |
%% +---+------+------+---------------------+------+------+------+
%% | 1 | aaa  | NULL | NULL                | NULL | NULL | NULL |
%% | 2 | NULL | NULL | NULL                | NULL | NULL | NULL |
%% | 3 |      | NULL | NULL                | NULL | NULL | NULL |
%% | 4 | NULL | 3.14 | NULL                | NULL | NULL | NULL |
%% | 5 | NULL | NULL | 2012-02-02 17:10:52 | NULL | NULL | NULL |
%% +---+------+------+---------------------+------+------+------+
%% fc=7, nf=(7+7+2)/8=2                         batd vk            c
%%            a={ (4+2)/8+1=1, 1<<6=(4+2)&7= 64=0100 0000, 0000 0000 }
%%            b={ (5+2)/8+1=1, 1<<7=(5+2)&7=128=1000 0000, 0000 0000 }
%%            c={ (6+2)/8+1=2, 1<<0=(6+2)&7=  1=0000 0000, 0000 0001 }
%% 1=<<0,240,1,1,0,0,0,3,97,97,97>> 0, 1111-0000,0000-0001, 1,0,0,0, 3,97,97,97

%% == public ==

-spec prepare_fields(fields()) -> fields().
prepare_fields(Fields) ->
    lists:map(fun(#field{type=T}=F) -> F#field{cast = cast(T)} end, Fields).


-spec recv_row(handle(),byte(),fields()) -> {ok, rows(), handle()}.
recv_row(#handle{}=H, <<0>>, Fields) -> % TODO
    Size = (length(Fields) + (8+1)) div 8,
    case recv_binary(Size, H) of
        {ok, Binary, Handle} ->
            recv_row(null_fields(Binary,0,Size,[]), Fields, [], Handle)
    end.

recv_row(_NullFields, [], List, Handle) ->
    {ok, lists:reverse(List), Handle};
recv_row([1|L], [_|T], List, Handle) ->
    recv_row(L, T, [null|List], Handle);
recv_row([0|L], [F|T], List, Handle) ->
    case restore(F, Handle) of
        {ok, V, H} ->
            recv_row(L, T, [V|List], H)
    end.

%% == internal ==

null_fields(_Binary, _Start, 0, List) ->
    lists:sublist(List, 3, length(List));
null_fields(Binary, Start, Length, List) ->
    <<B8:1,B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1>> = binary_part(Binary, Start, 1),
    null_fields(Binary, Start+1, Length-1, lists:append(List,[B1,B2,B3,B4,B5,B6,B7,B8])).

restore(#field{cast={integer,Size},flags=F}, #handle{}=H) ->
    case recv_binary(Size, H) of
        {ok, Binary, Handle} ->
            Data = case ?IS_SET(F, ?UNSIGNED_FLAG) of
                       true  -> <<Value:Size/integer-unsigned-little-unit:8>> = Binary, Value;
                       false -> <<Value:Size/integer-signed-little-unit:8>> = Binary, Value
                   end,
            {ok, Data, Handle}
    end;
restore(#field{cast={float,Size},flags=F}, #handle{}=H) ->
    case recv_binary(Size, H) of
        {ok, Binary, Handle} ->
            Data = case ?IS_SET(F, ?UNSIGNED_FLAG) of
                       true  -> <<Value:Size/float-unsigned-little-unit:8>> = Binary, Value;
                       false -> <<Value:Size/float-signed-little-unit:8>> = Binary, Value
                   end,
            {ok, Data, Handle}
    end;
restore(#field{cast=C}=F, #handle{}=H) ->
    case recv_packed_binary(undefined, H) of
        {ok, Binary, Handle} ->
            {ok, C(Binary,F), Handle}
    end.


%%st(?MYSQL_TYPE_DECIMAL)     -> undefined
cast(?MYSQL_TYPE_TINY)        -> {integer,1};
cast(?MYSQL_TYPE_SHORT)       -> {integer,2};
cast(?MYSQL_TYPE_LONG)        -> {integer,4};
cast(?MYSQL_TYPE_FLOAT)       -> {float,4};
cast(?MYSQL_TYPE_DOUBLE)      -> {float,8};
%%st(?MYSQL_TYPE_NULL)        -> undefined
cast(?MYSQL_TYPE_TIMESTAMP)   -> fun to_datetime/2;
cast(?MYSQL_TYPE_LONGLONG)    -> {integer,8};
cast(?MYSQL_TYPE_INT24)       -> {integer,4};
cast(?MYSQL_TYPE_DATE)        -> fun to_date/2;
cast(?MYSQL_TYPE_TIME)        -> fun to_time/2;
cast(?MYSQL_TYPE_DATETIME)    -> fun to_datetime/2;
cast(?MYSQL_TYPE_YEAR)        -> {integer,2};
%%st(?MYSQL_TYPE_NEWDATE)     -> undefined
%%st(?MYSQL_TYPE_VARCHAR)     -> undefined
cast(?MYSQL_TYPE_BIT)         -> fun to_bit/2;
%%st(?MYSQL_TYPE_TIMESTAMP2)  -> undefined
%%st(?MYSQL_TYPE_DATETIME2)   -> undefined
%%st(?MYSQL_TYPE_TIME2)       -> undefined
%%st(?MYSQL_TYPE_JSON)        -> undefined
cast(?MYSQL_TYPE_NEWDECIMAL)  -> fun to_decimal/2;
%%st(?MYSQL_TYPE_ENUM)        -> undefined
%%st(?MYSQL_TYPE_SET)         -> undefined
%%st(?MYSQL_TYPE_TINY_BLOB)   -> undefined
%%st(?MYSQL_TYPE_MEDIUM_BLOB) -> undefined
%%st(?MYSQL_TYPE_LONG_BLOB)   -> undefined
cast(?MYSQL_TYPE_BLOB)        -> fun to_binary/2;
cast(?MYSQL_TYPE_VAR_STRING)  -> fun to_binary/2;
cast(?MYSQL_TYPE_STRING)      -> fun to_binary/2.
%%st(?MYSQL_TYPE_GEOMETRY)    -> undefined


to_bit(Binary, _Field) ->
    binary:decode_unsigned(Binary, big).

to_binary(Binary, _Field) ->
    Binary.

to_date(Binary, _Field) ->
    <<Year:16/little,Month,Day>> = Binary,
    {Year,Month,Day}.

to_datetime(Binary, _Field) ->
    case Binary of
        <<Year:16/little,Month,Day,Hour,Minute,Second>> ->
            {{Year,Month,Day},{Hour,Minute,Second}}
            %%
            %% TODO: second_part, 7->11?
            %%
    end.

to_decimal(Binary, #field{decimals=D}) ->
    binary_to_float(Binary, D).

to_time(Binary, _Time) ->
    case Binary of
        <<_Neg,_Day:32/little,Hour,Minute,Second>> ->
            {Hour,Minute,Second}
            %%
            %% TODO: second_part, 8->12?
            %%
    end.
