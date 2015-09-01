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

-module(myer_socket).

-include("internal.hrl").

%% -- public --
-export([connect/4, close/1]).
-export([recv/1]).
-export([recv/3, send/3]).
-export([reset/1, zreset/1]).

%% -- internal --
-record(handle, {
          socket        :: tuple(),             % baseline_socket:socket()
          maxlength     :: non_neg_integer(),
          timeout       :: timeout(),
          seqnum = 0    :: non_neg_integer(),
          buf    = <<>> :: binary(),
          start  = 0    :: non_neg_integer(),
          length = 0    :: non_neg_integer(),

          zseqnum = 0 :: non_neg_integer(),
          zraw = <<>> :: binary()
         }).

-type(handle() :: #handle{}).

%% == public ==

-spec connect(inet:ip_address()|inet:hostname(),inet:port_number(),
              non_neg_integer(),timeout()) -> {ok,handle()}|{error,_}.
connect(Address, Port, MaxLength, Timeout) ->
    L = [
         {active, false},
         {keepalive, true},
         binary,
         {packet, raw},
         {packet_size, MaxLength} % TODO
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


-spec recv(handle()) -> {ok,binary(),handle()}|{error,_}.
recv(#handle{socket=S,timeout=T,seqnum=N}=H) ->
    case baseline_socket:recv(S, 4, T) of
        {ok, <<L:24/little,N>>, S1} ->
            case baseline_socket:recv(S1, L, T) of
                {ok, Packet, S2} ->
                    {ok, Packet, H#handle{socket = S2, seqnum = N+1,
                                          buf = <<>>, start = 0, length = 0}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec recv(handle(),non_neg_integer()|binary:cp(),boolean())
          -> {ok,binary(),handle()}|{error,_}.
recv(#handle{buf=B,start=S,length=L}=H, 0, _Compress) ->
    {ok, binary_part(B,S,L), H#handle{buf = <<>>, start = 0, length = 0}};
recv(Handle, Term, false)
  when is_integer(Term) ->
    recv_binary(Handle, Term);
recv(Handle, Term, false) ->
    recv_text(Handle, Term).

-spec send(handle(),binary(),boolean()) -> {ok,handle()}|{error,_}.
send(Handle, Binary, Compress) ->
    send(Handle, Binary, 0, byte_size(Binary), Compress).


-spec reset(handle()) -> handle().
reset(#handle{}=H) ->
    H#handle{seqnum = 0, zseqnum = 0}.

-spec zreset(handle()) -> handle().
zreset(#handle{seqnum=N}=H) ->
    H#handle{zseqnum = N}.

%% == internal ==

update(Handle, Binary, Length) ->
    case recv(Handle) of
        {ok, Packet, #handle{}=H} ->
            {ok, H#handle{buf = <<Binary/binary,Packet/binary>>,
                          start = 0, length = Length+byte_size(Packet)}};
        {error, Reason} ->
            {error, Reason}
    end.

recv_binary(#handle{length=L}=H, Length) ->
    recv_binary(H, Length, L >= Length).

recv_binary(#handle{buf=B,start=S,length=L}=H, Length, true) ->
    {ok, binary_part(B,S,Length), H#handle{start = S+Length, length = L-Length}};
recv_binary(#handle{buf=B,start=S,length=L}=H, Length, false) ->
    case update(H, binary_part(B,S,L), L) of
        {ok, Handle} ->
            recv_binary(Handle, Length);
        {error, Reason} ->
            {error, Reason}
    end.

recv_text(#handle{buf=B,start=S,length=L}=H, Pattern) ->
    Binary = binary_part(B, S, L),
    recv_text(H, Pattern, Binary, L, binary:match(Binary,Pattern)).

recv_text(#handle{start=S}=H, _Pattern, Binary, Length, {MS,ML}) ->
    {ok, binary_part(Binary,0,MS), H#handle{start = S+(MS+ML), length = Length-(MS+ML)}};
recv_text(#handle{}=H, Pattern, Binary, Length, nomatch) ->
    case update(H, Binary, Length) of
        {ok, Handle} ->
            recv_text(Handle, Pattern);
        {error, Reason} ->
            {error, Reason}
    end.

send(#handle{socket=S,maxlength=M,seqnum=N}=H, Binary, Start, Length, false)
  when M >= Length ->
    B = binary_part(Binary, Start, Length),
    case baseline_socket:send(S, <<Length:24/little,N,B/binary>>) of
        ok ->
            {ok, H#handle{seqnum = N+1}};
        {error, Reason} ->
            {error, Reason}
    end;
send(_Handle, _Binary, _Start, _Length, true) ->
    {error, notimplemented};
send(#handle{maxlength=M}=H, Binary, Start, Length, Compress) ->
    case send(H, Binary, Start, M, Compress) of
        {ok, Handle} ->
            send(Handle, Binary, Start+M, Length-M, Compress);
        {error, Reason} ->
            {error, Reason}
    end.
