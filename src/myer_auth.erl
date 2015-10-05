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

-module(myer_auth).

-include("internal.hrl").

%% -- private --
-export([auth_to_binary/3, auth_to_binary/5]).
-export([recv_handshake/2, recv_plugin/1]).
-export([caps/1, version/1]).

%% -- internal --
-import(myer_handle, [recv_binary/2, recv_text/2]).
-import(myer_protocol, [recv_unsigned/2]).

-record(handshake, {
          version   :: version(),
          tid       :: non_neg_integer(),
          seed1     :: binary(),
          caps1     :: non_neg_integer(),
          charset   :: non_neg_integer(),
          status    :: non_neg_integer(),
          caps2     :: non_neg_integer(),
          length    :: non_neg_integer(),
          reserved  :: binary(),
          seed2     :: binary(),
          plugin    :: binary(),
          maxlength :: non_neg_integer(),
          seed      :: binary(),
          caps      :: integer()
         }).

-record(plugin, {
          name :: binary(),
          data :: binary()
         }).

-type(handle() :: myer_handle:handle()).
-type(handshake() :: #handshake{}).
-type(plugin() :: #plugin{}).

-export_type([handshake/0, plugin/0]).

%% == private ==

-spec auth_to_binary(binary(),plugin(),handshake()) -> {ok,binary()}.
auth_to_binary(Password, #plugin{name=N}, #handshake{seed=S}) ->
    Scrambled = scramble(Password, S, N),
    {ok, <<Scrambled/binary,0>>}.

-spec auth_to_binary(binary(),binary(),binary(),non_neg_integer(),handshake()) ->
                            {ok,binary(),handshake()}.
auth_to_binary(User, Password, <<>>, Charset, #handshake{caps=C}=H) ->
    Handshake = H#handshake{caps = (C bxor ?CLIENT_CONNECT_WITH_DB), charset = Charset},
    {ok, auth_to_binary(User,Password,<<>>,Handshake),Handshake};
auth_to_binary(User, Password, Database, Charset, #handshake{caps=C}=H) ->
    Handshake = H#handshake{caps = (C bxor ?CLIENT_NO_SCHEMA), charset = Charset},
    {ok, auth_to_binary(User,Password,Database,Handshake),Handshake}.


-spec recv_handshake(non_neg_integer(),handle()) -> {ok,handshake(),handle()}.
recv_handshake(MaxLength, Handle) ->
    recv_handshake(#handshake{maxlength = MaxLength}, myer_handle:caps(Handle), Handle).

-spec recv_plugin(handle()) -> {ok,plugin(),handle()}.
recv_plugin(Handle) ->
    recv_plugin(#plugin{}, myer_handle:caps(Handle), Handle).


-spec caps(handshake()) -> non_neg_integer().
caps(#handshake{caps=C}) ->
    C.

-spec version(handshake()) -> version().
version(#handshake{version=C}) ->
    C.

%% == internal ==

binary_to_version(Binary) ->
    F = fun(E) -> try binary_to_integer(E,10) catch _:_ -> E end end,
    L = binary:split(<<Binary/binary,".0.0">>, [<<$.>>,<<$->>], [global]),
    lists:map(F, lists:sublist(L,3)).

scramble(<<>>, _Seed, <<"mysql_native_password">>) ->
    <<>>;
scramble(Password, Seed, <<"mysql_native_password">>) ->
    scramble(Password, Seed);
scramble(Password, _Seed, <<"auth_test_plugin">>) ->
    Password;
scramble(<<>>, _Seed, _Plugin) ->
    <<0:64>>;
scramble(Password, Seed, _Plugin) ->
    scramble_323(Password, Seed).

%% -----------------------------------------------------------------------------
%% << sql/password.c : scramble/3, check_scramble/3
%% -----------------------------------------------------------------------------
scramble(Password, Seed) ->
    R = crypto:hash(sha, Password),
    L = crypto:hash_final(
          crypto:hash_update(
            crypto:hash_update(crypto:hash_init(sha), Seed),
            crypto:hash(sha, R)
           )
         ),
    list_to_binary([ binary:at(L,P) bxor binary:at(R,P) || P <- lists:seq(0,size(R)-1) ]).

%% -----------------------------------------------------------------------------
%% << sql/password.c : scramble_323/3, check_scramble_323/3
%% -----------------------------------------------------------------------------
scramble_323(Password, Seed) when is_list(Password), is_list(Seed), length(Seed) > 7 ->
    Hash = fun(S) ->
                   {L,R,_} = lists:foldl(
                               fun(E, {L,R,A}) when E =/= 16#20, E =/= 16#09 ->
                                       T = L bxor (((L band 63) + A) * E + (L bsl 8)),
                                       {T, R + ((R bsl 8) bxor T), A + E};
                                  (_, A) ->
                                       A
                               end,
                               {1345345333, 16#12345671, 7},
                               S
                              ),
                   {L band ((1 bsl 31) -1), R band ((1 bsl 31) -1)}
           end,
    Rand = fun({{PL,PR}, {SL,SR}}) ->
                   X = 16#3FFFFFFF,
                   {(PL bxor SL) rem X, (PR bxor SR) rem X, X};
              ({L,R,X}) ->
                   NL = (L * 3 + R) rem X,
                   NR = (NL + R + 33) rem X,
                   {NL / X, {NL,NR,X}}
           end,
    S = lists:sublist(Seed, 8),
    {NV, L} = lists:foldl(
                fun(_, {V,A}) ->
                        {T,NV} = Rand(V),
                        {NV, [(trunc(T * 31) + 64)|A]}
                end,
                {Rand({Hash(Password), Hash(S)}), []},
                lists:seq(1, 8)
               ),
    {T, _} = Rand(NV),
    B = trunc(T * 31),
    list_to_binary([E bxor B || E <- lists:reverse(L)]);
scramble_323(Password, Seed) when is_binary(Password) ->
    scramble_323(binary_to_list(Password), Seed);
scramble_323(Password, Seed) when is_binary(Seed) ->
    scramble_323(Password, binary_to_list(Seed)).

%% -----------------------------------------------------------------------------
%% << sql-common/client.c : send_client_reply_packet/3
%% -----------------------------------------------------------------------------
auth_to_binary(User, Password, Database,
               #handshake{maxlength=M,seed=S,caps=C,charset=E,plugin=P})
  when ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    X = scramble(Password, S, P),
    B = if ?IS_SET(C,?CLIENT_SECURE_CONNECTION) -> N = size(X), <<N,X/binary>>;
           true                                 -> <<0>>
        end,
    D = if ?IS_SET(C,?CLIENT_CONNECT_WITH_DB) -> <<Database/binary,0>>;
           true                               -> <<>>
        end,
    A = if ?IS_SET(C,?CLIENT_PLUGIN_AUTH) -> <<P/binary,0>>;
           true                           -> <<>>
        end,
    <<
      C:32/little,
      M:32/little,
      E,
      0:23/integer-unit:8,
      User/binary, 0,
      B/binary,
      D/binary,
      A/binary
    >>;
auth_to_binary(User, Password, Database,
               #handshake{maxlength=M,seed=S,caps=C,plugin=P}) ->
    A = (C bor ?CLIENT_LONG_PASSWORD) band 16#ffff, % FORCE
    X = scramble(Password, S, P),
    B = if ?IS_SET(C,?CLIENT_SECURE_CONNECTION) -> N = size(X), <<N,X/binary>>;
           true                                 -> <<0>>
        end,
    D = if ?IS_SET(C,?CLIENT_CONNECT_WITH_DB) -> <<Database/binary,0>>;
           true                               -> <<>>
        end,
    <<
      A:16/little,
      M:24/little,
      User/binary, 0,
      B/binary,
      D/binary
    >>.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_server_handshake_packet/3
%%    sql/auth/sql_authentication.cc : send_server_handshake_packet/3
%% -----------------------------------------------------------------------------
recv_handshake(#handshake{version=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{version = binary_to_version(B)}, Caps, H);
recv_handshake(#handshake{tid=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(4, Handle),
    recv_handshake(R#handshake{tid = U}, Caps, H);
recv_handshake(#handshake{seed1=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{seed1 = B}, Caps, H);
recv_handshake(#handshake{caps1=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_handshake(R#handshake{caps1 = U}, Caps, H);
recv_handshake(#handshake{charset=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(1, Handle),
    recv_handshake(R#handshake{charset = U}, Caps, H);
recv_handshake(#handshake{status=undefined}=R, Caps, Handle) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_handshake(R#handshake{status = U}, Caps, H);
recv_handshake(#handshake{version=V,caps1=C,caps2=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(2, Handle),
    recv_handshake(R#handshake{caps2 = U}, Caps, H);
recv_handshake(#handshake{caps2=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{caps2 = 0}, Caps, Handle);
recv_handshake(#handshake{version=V,caps1=C,length=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, U, H} = recv_unsigned(1, Handle),
    recv_handshake(R#handshake{length = U}, Caps, H);
recv_handshake(#handshake{length=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{length = 8}, Caps, Handle);   % 8?, TODO
recv_handshake(#handshake{version=V,caps1=C,reserved=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_binary(10, Handle),
    recv_handshake(R#handshake{reserved = B}, Caps, H); % "always 0"
recv_handshake(#handshake{reserved=undefined}=R, Caps, Handle) ->
    {ok, B, H} = recv_binary(13, Handle),
    recv_handshake(R#handshake{reserved = B}, Caps, H); % "always 0"?
recv_handshake(#handshake{version=V,caps1=C,seed2=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{seed2 = B}, Caps, H);
recv_handshake(#handshake{seed2=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{seed2 = <<>>}, Caps, Handle);
recv_handshake(#handshake{version=V,caps1=C,plugin=undefined}=R, Caps, Handle)
  when V >= [4,1,1]; ?IS_SET(C,?CLIENT_PROTOCOL_41) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_handshake(R#handshake{plugin = B}, Caps, H);
recv_handshake(#handshake{plugin=undefined}=R, Caps, Handle) ->
    recv_handshake(R#handshake{plugin = <<"mysql_native_password">>}, Caps, Handle);
recv_handshake(#handshake{caps1=C1,caps2=C2,seed1=S1,seed2=S2}=R, Caps, Handle) ->
    {ok, R#handshake{caps = Caps band ((C2 bsl 16) bor C1),
                     seed = <<S1/binary,S2/binary>>}, Handle}.

%% -----------------------------------------------------------------------------
%% << sql/sql_acl.cc (< 5.7)         : send_plugin_request_packet/3
%%    sql/auth/sql_authentication.cc : send_plugin_request_packet/3
%% -----------------------------------------------------------------------------
recv_plugin(#plugin{name=undefined}=P, Caps, Handle) ->
    {ok, B, H} = recv_text(<<0>>, Handle),
    recv_plugin(P#plugin{name = B}, Caps, H);
recv_plugin(#plugin{}=P, _Caps, Handle) ->
    {ok, B, H} = recv_binary(?REMAINS(Handle), Handle), % TODO
    {ok, P#plugin{data = B}, H}.
