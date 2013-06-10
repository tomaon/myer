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

-module(myer_network).

-include("myer_internal.hrl").

%% -- public --
-export([connect/4, close/1]).
-export([recv/3, send/3]).
-export([reset/1, zreset/1]).

%% -- private --
-record(handle, {
          socket :: socket(),
          maxlength :: non_neg_integer(),
          timeout :: timeout(),
          seqnum = 0 :: non_neg_integer(),
          zseqnum = 0 :: non_neg_integer(),
          buf = <<>> :: binary(),
          buf_size = 0 :: non_neg_integer(),
          raw = <<>> :: binary(),
          zraw = <<>> :: binary()
         }).

-type(handle() :: #handle{}).

-define(MAX_PACKET_LENGTH, 16777215).

%% == public ==

-spec connect(ip_address()|hostname(),port_number(),non_neg_integer(),timeout())
             -> {ok,handle()}|{error,_}.
connect(Address, Port, MaxLength, Timeout) ->
    L = [
         {active, false},
         {keepalive, true},
         binary,
         {packet, raw},
         {packet_size, MaxLength} % ??
        ],
    try gen_tcp:connect(Address, Port, L, Timeout) of
        {ok, Socket} ->
            {ok, #handle{socket = Socket, maxlength = MaxLength, timeout = Timeout}};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close(handle()) -> ok.
close(#handle{socket=S}) ->
    gen_tcp:close(S).

-spec recv(handle(),non_neg_integer(),boolean()) -> {ok,binary(),handle()}|{error,_,handle()}.
recv(#handle{buf=B}=H, 0, _Compress) ->
    {ok, B, H#handle{buf = <<>>, buf_size = 0}};
recv(Handle, Length, Compress) ->
    buffered(Handle, Length, Compress, Length, []).

-spec send(handle(),binary(),boolean()) -> {ok,handle()}|{error,_,handle()}.
send(#handle{}=H, Binary, Compress)
  when ?MAX_PACKET_LENGTH =< size(Binary) ->
    {B, R} = split_binary(Binary, ?MAX_PACKET_LENGTH),
    case send(H, B, ?MAX_PACKET_LENGTH, Compress) of
        {ok, Handle} ->
            send(Handle, R, Compress);
        {error, Reason, Handle} ->
            {error, Reason, Handle}
    end;
send(Handle, Binary, Compress) ->
    send(Handle, Binary, size(Binary), Compress).

-spec reset(handle()) -> handle().
reset(#handle{}=H) ->
    H#handle{seqnum = 0, zseqnum = 0}.

-spec zreset(handle()) -> handle().
zreset(#handle{seqnum=N}=H) ->
    H#handle{zseqnum = N}.

%% == private ==

buffered(Handle, _Length, _Compress, 0, List) ->
    {ok, iolist_to_binary(lists:reverse(List)), Handle};
buffered(#handle{buf_size=S}=H, Length, Compress, Left, List)
  when S < Left ->
    case dispatch(H, Compress) of
        {ok, Handle} ->
            buffered(Handle, Length, Compress, Left, List);
        {error, Reason, Handle} ->
            {error, Reason, Handle}
    end;
buffered(#handle{buf=B,buf_size=S}=H, Length, Compress, Left, List)
  when S > Left ->
    {Binary, Rest} = split_binary(B, Left),
    buffered(H#handle{buf = Rest, buf_size = size(Rest)},
             Length, Compress, 0, [Binary|List]);
buffered(#handle{buf=B,buf_size=S}=H, Length, Compress, Left, List) ->
    buffered(H#handle{buf = <<>>, buf_size = 0},
             Length, Compress, Left - S, [B|List]).

dispatch(Handle, false) -> recv(Handle);
dispatch(#handle{zraw=Z}=H, true)  -> recv(H, Z).

recv(#handle{seqnum= N,raw = <<L:24/little,N,B/binary>>}=H)
  when L =< size(B) ->
    {Binary, Rest} = split_binary(B, L),
    {ok, H#handle{seqnum = N+1, buf = Binary, buf_size = L, raw = Rest}};
recv(#handle{socket=S,timeout=T,raw=R}=H) ->
    case gen_tcp:recv(S, 0, T) of
        {ok, Packet} ->
            recv(H#handle{raw = iolist_to_binary([R,Packet])});
        {error, Reason} ->
            {error, Reason, H}
    end.

recv(#handle{zseqnum=N,raw= <<L:24/little,N,B/binary>>}=H, ZRaw)
  when L =< size(B) ->
    {Binary, Rest} = split_binary(B, L),
    {ok, H#handle{zseqnum = N+1, buf = Binary, buf_size = L, raw = Rest, zraw = ZRaw}};
recv(#handle{seqnum=N,raw=R}=H, <<L:24/little,N,Z:24/little,B/binary>>)
  when L =< 3 + size(B) ->
    {Binary, Rest} = split_binary(B, L),
    X = if 0 < Z -> zlib:uncompress(Binary); true -> Binary end,
    recv(H#handle{seqnum = N+1, raw = iolist_to_binary([R,X])}, Rest);
recv(#handle{socket=S,timeout=T}=H, ZRaw) ->
    case gen_tcp:recv(S, 0, T) of
        {ok, Packet} ->
            recv(H, iolist_to_binary([ZRaw,Packet]));
	{error, Reason} ->
	    {error, Reason, H}
    end.

send(#handle{zseqnum=N}=H, []) ->
    {ok, H#handle{seqnum = N}};
send(#handle{socket=S,zseqnum=N}=H, [OB|T]) ->
    OL = size(OB),
    ZB = zlib:compress(OB),
    ZL = size(ZB),
    B = if  ZL < OL -> <<ZL:24/little, N, OL:24/little, ZB/binary>>;
            true    -> <<OL:24/little, N, 0:24, OB/binary>>
        end,
    case gen_tcp:send(S, B) of
        ok ->
            send(H#handle{zseqnum = N+1}, T);
        {error, Reason} ->
            {error, Reason, H}
    end.

send(#handle{socket=S,seqnum=N}=H, Binary, Size, false) ->
    case gen_tcp:send(S, <<Size:24/little,N,Binary/binary>>) of
        ok ->
            {ok, H#handle{seqnum = N+1}};
        {error, Reason} ->
	    {error, Reason, H}
    end;
send(#handle{seqnum=N}=H, Binary, Size, true) ->
    L = if ?MAX_PACKET_LENGTH - 4 >= Size -> [<<Size:24/little,N,Binary/binary>>];
           true                           -> [<<Size:24/little,N>>, Binary]
        end,
    send(H#handle{seqnum = N+1, zseqnum = N}, L).
