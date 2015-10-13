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

-module(myer_handle).

-include("internal.hrl").

%% -- private --
-export([connect/4, close/1]).
-export([recv_binary/2, recv_text/2, send/2]).
-export([reset/1]).
-export([caps/1, caps/2, version/1, version/2]).

%% -- internal --
-record(handle, {
          socket        :: tuple(),             % baseline_socket:socket()
          maxlength     :: non_neg_integer(),
          timeout       :: timeout(),
          send          :: function(),
          recv          :: function(),
          caps          :: non_neg_integer(),
          version       :: [non_neg_integer()],
          seqnum = 0    :: non_neg_integer(),
          zseqnum = 0   :: non_neg_integer(),   % TODO
          buf    = <<>> :: binary(),
          start  = 0    :: non_neg_integer(),
          length = 0    :: non_neg_integer()
         }).

-type(handle() :: #handle{}).

-export_type([handle/0]).

%% == private ==

-spec connect(inet:ip_address()|inet:hostname(),inet:port_number(),
              non_neg_integer(),timeout()) -> {ok,handle()}|{error,_}.
connect(Address, Port, MaxLength, Timeout) ->
    L = [
         {active, false},
         {keepalive, true},
         {mode, binary},
         {packet, raw},
         {packet_size, 4+MaxLength}, % TODO
         {send_timeout, timer:seconds(?NET_WRITE_TIMEOUT)},
         {send_timeout_close, true}
        ],
    case baseline_socket:connect(Address, Port, L, Timeout) of
        {ok, Socket} ->
            {ok, #handle{socket = Socket, maxlength = MaxLength, timeout = Timeout,
                         send = fun send_normal/2, recv = fun recv_normal/3}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec close(handle()) -> ok.
close(#handle{socket=S}) ->
    baseline_socket:close(S).


-spec recv_binary(non_neg_integer(),handle()) -> {ok,binary(),handle()}.
recv_binary(Length, #handle{length=L}=H) ->
    recv_binary(H, Length, Length =< L).

-spec recv_text(binary()|binary:cp(),handle()) -> {ok,binary(),handle()}.
recv_text(Pattern, #handle{buf=B,start=S,length=L}=H) ->
    Binary = binary_part(B, S, L),
    recv_text(H, Pattern, Binary, L, binary:match(Binary,Pattern)).

-spec send(binary(),handle()) -> {ok,handle()}.
send(Binary, #handle{}=H) ->
    send(H, Binary, 0, byte_size(Binary)).


-spec reset(handle()) -> handle().
reset(#handle{caps=C}=H)
  when ?IS_SET(C, ?CLIENT_COMPRESS) ->
    H#handle{send = fun send_compress/2, recv = fun recv_compress/3, seqnum = 0, zseqnum = 0};
reset(#handle{}=H) ->
    H#handle{send = fun send_normal/2, recv = fun recv_normal/3, seqnum = 0, zseqnum = 0}.


-spec caps(handle()) -> non_neg_integer().
caps(#handle{caps=C}) ->
    C.

-spec caps(handle(),non_neg_integer()) -> handle().
caps(#handle{}=H, Caps) ->
    H#handle{caps = Caps}.

-spec version(handle()) -> version().
version(#handle{version=V}) ->
    V.

-spec version(handle(),version()) -> handle().
version(#handle{}=H, Version) ->
    H#handle{version = Version}.

%% == internal ==

unpack(Binary, Start, Length, _N, List)
  when 4 >= Length ->
    list_to_binary(lists:reverse([binary_part(Binary,Start,Length)|List]));
unpack(Binary, Start, Length, N, List) ->
    <<L:24/little,N>> = binary_part(Binary, Start, 4),
    unpack(Binary, Start+(4+L), Length-(4+L), N+1, [binary_part(Binary,Start+4,L)|List]).

recv_compress(#handle{socket=S,timeout=T,zseqnum=N}=H, Binary, Length) ->
    case baseline_socket:recv(S, 7, T) of
        {ok, <<L:24/little,N,Z:24/little>>, S1} ->
            case baseline_socket:recv(S1, L, T) of
                {ok, Packet, S2} ->

                    {Raw, Size} = if 0 < Z -> B = zlib:uncompress(Packet), {B, Z};
                                     true  -> {Packet, L}
                                  end,

                    Buf = unpack(<<Binary/binary,Raw/binary>>, 0, Length+Size, N, []),

                    {ok, H#handle{socket = S2, zseqnum = N+1,
                                  buf = Buf, start = 0, length = byte_size(Buf)}};

                {error, Reason, S2} ->
                    throw({error,Reason,#handle{socket = S2}})
            end;
        {error, Reason, S1} ->
            throw({error,Reason,#handle{socket = S1}})
    end.

recv_normal(#handle{socket=S,timeout=T,seqnum=N}=H, Binary, Length) ->
    case baseline_socket:recv(S, 4, T) of
        {ok, <<L:24/little,N>>, S1} ->
            case baseline_socket:recv(S1, L, T) of
                {ok, Packet, S2} ->
                    {ok, H#handle{socket = S2, seqnum = N+1,
                                  buf = <<Binary/binary,Packet/binary>>,
                                  start = 0, length = Length+byte_size(Packet)}};
                {error, Reason, S2} ->
                    throw({error,Reason,#handle{socket = S2}})
            end;
        {error, Reason, S1} ->
            throw({error,Reason,#handle{socket = S1}})
    end.

recv_binary(#handle{buf=B,start=S,length=L}=H, Length, true) ->
    {ok, binary_part(B,S,Length), H#handle{start = S+Length, length = L-Length}};
recv_binary(#handle{recv=R,buf=B,start=S,length=L}=H, Length, false) ->
    case R(H, binary_part(B,S,L), L) of
        {ok, Handle} ->
            recv_binary(Length, Handle) % CAUTION
    end.

recv_text(#handle{start=S}=H, _Pattern, Binary, Length, {MS,ML}) ->
    {ok, binary_part(Binary,0,MS), H#handle{start = S+(MS+ML), length = Length-(MS+ML)}};
recv_text(#handle{recv=R}=H, Pattern, Binary, Length, nomatch) ->
    case R(H, Binary, Length) of
        {ok, Handle} ->
            recv_text(Pattern, Handle) % CAUTION
    end.

send(#handle{maxlength=M,send=S,seqnum=N}=H, Binary, Start, Length) % TODO: pack
  when M >= 4+Length ->
    B = binary_part(Binary, Start, Length),
    S(H, <<Length:24/little,N,B/binary>>);
send(#handle{maxlength=M}=H, Binary, Start, Length) ->
    case send(H, Binary, Start, M) of
        {ok, Handle} ->
            send(Handle, Binary, Start+M, Length-M)
    end.

send_compress(#handle{socket=S,zseqnum=N}=H, Binary) ->
    BS = byte_size(Binary),
    ZB = zlib:compress(Binary),
    ZS = byte_size(ZB),
    B = if BS > ZS -> <<ZS:24/little, N, BS:24/little, ZB/binary>>;
           true    -> <<BS:24/little, N, 0:24, Binary/binary>>
        end,
    case baseline_socket:send(S, B) of
        ok ->
            {ok, H#handle{zseqnum = N+1}};
        {error, Reason} ->
            throw({error,Reason,H})
    end.

send_normal(#handle{socket=S,seqnum=N}=H, Binary) ->
    case baseline_socket:send(S, Binary) of
        ok ->
            {ok, H#handle{seqnum = N+1}};
        {error, Reason} ->
            throw({error,Reason,H})
    end.
