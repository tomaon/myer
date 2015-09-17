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

%% -- public --
-export([connect/4, close/1]).
-export([recv_binary/2, recv_text/2, send/2]).
-export([reset/1]).
-export([caps/1, caps/2, version/1, version/2]).

%% -- internal --

-record(handle, {
          socket        :: tuple(),             % baseline_socket:socket()
          maxlength     :: non_neg_integer(),
          timeout       :: timeout(),
          caps          :: non_neg_integer(),
          version       :: [non_neg_integer()],
          seqnum = 0    :: non_neg_integer(),
          buf    = <<>> :: binary(),
          start  = 0    :: non_neg_integer(),
          length = 0    :: non_neg_integer()
         }).

-type(handle() :: #handle{}).

-export_type([handle/0]).

%% == public ==

-spec connect(inet:ip_address()|inet:hostname(),inet:port_number(),
              non_neg_integer(),timeout()) -> {ok,handle()}|{error,_}.
connect(Address, Port, MaxLength, Timeout) ->
    L = [
         {active, false},
         {keepalive, true},
         {mode, binary},
         {packet, raw},
         {packet_size, 3+MaxLength}, % TODO
         {send_timeout, timer:seconds(?NET_WRITE_TIMEOUT)},
         {send_timeout_close, true}
        ],
    case baseline_socket:connect(Address, Port, L, Timeout) of
        {ok, Socket} ->
            {ok, #handle{socket = Socket, maxlength = MaxLength, timeout = Timeout}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec close(handle()) -> ok.
close(#handle{socket=S}) ->
    baseline_socket:close(S).


-spec recv_binary(non_neg_integer(),handle()) -> {ok,binary(),handle()}.
recv_binary(Length, #handle{length=L}=H) ->
    recv_binary(H, Length, L >= Length).

-spec recv_text(binary()|binary:cp(),handle()) -> {ok,binary(),handle()}.
recv_text(Pattern, #handle{buf=B,start=S,length=L}=H) ->
    Binary = binary_part(B, S, L),
    recv_text(H, Pattern, Binary, L, binary:match(Binary,Pattern)).

-spec send(binary(),handle()) -> {ok,handle()}.
send(Binary, #handle{}=H) ->
    send(H, Binary, 0, byte_size(Binary)).


-spec reset(handle()) -> handle().
reset(#handle{}=H) ->
    H#handle{seqnum = 0}.


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

update(Handle, Binary, Length) ->
    case recv(Handle) of
        {ok, Packet, #handle{}=H} ->
            {ok, H#handle{buf = <<Binary/binary,Packet/binary>>,
                          start = 0, length = Length+byte_size(Packet)}}
    end.

recv(#handle{socket=S,timeout=T,seqnum=N}=H) ->
    case baseline_socket:recv(S, 4, T) of
        {ok, <<L:24/little,N>>, S1} ->
            case baseline_socket:recv(S1, L, T) of
                {ok, Packet, S2} ->
                    {ok, Packet, H#handle{socket = S2, seqnum = N+1,
                                          buf = <<>>, start = 0, length = 0}};
                {error, Reason, Handle} ->
                    throw({error,Reason,Handle})
            end;
        {error, Reason, Handle} ->
            throw({error,Reason,Handle})
    end.

recv_binary(#handle{buf=B,start=S,length=L}=H, Length, true) ->
    {ok, binary_part(B,S,Length), H#handle{start = S+Length, length = L-Length}};
recv_binary(#handle{buf=B,start=S,length=L}=H, Length, false) ->
    case update(H, binary_part(B,S,L), L) of
        {ok, Handle} ->
            recv_binary(Length, Handle) % CAUTION
    end.

recv_text(#handle{start=S}=H, _Pattern, Binary, Length, {MS,ML}) ->
    {ok, binary_part(Binary,0,MS), H#handle{start = S+(MS+ML), length = Length-(MS+ML)}};
recv_text(#handle{}=H, Pattern, Binary, Length, nomatch) ->
    case update(H, Binary, Length) of
        {ok, Handle} ->
            recv_text(Pattern, Handle) % CAUTION
    end.

send(#handle{socket=S,maxlength=M,seqnum=N}=H, Binary, Start, Length)
  when M >= Length ->
    B = binary_part(Binary, Start, Length),
    case baseline_socket:send(S, <<Length:24/little,N,B/binary>>) of
        ok ->
            {ok, H#handle{seqnum = N+1}};
        {error, Reason, Handle} ->
            throw({error,Reason,Handle})
    end;
send(#handle{maxlength=M}=H, Binary, Start, Length) ->
    case send(H, Binary, Start, M) of
        {ok, Handle} ->
            send(Handle, Binary, Start+M, Length-M)
    end.
