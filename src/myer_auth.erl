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

%% -- public --
-export([scramble/3]).

%% == public ==

scramble(<<>>, _Seed, <<"mysql_native_password">>) ->
    <<>>;
scramble(Password, Seed, <<"mysql_native_password">>) ->
    scramble(Password, Seed);
scramble(<<>>, _Seed, _Plugin) ->
    <<0:64>>;
scramble(Password, Seed, _Plugin) ->
    scramble_323(Password, Seed).

%% == private ==

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
