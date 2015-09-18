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

-module(myer_app).

-include("internal.hrl").

%% -- behaviour: application --
-behaviour(application).
-export([start/2, stop/1]).

%% == behaviour: application ==

start(_StartType, StartArgs) ->
    baseline_sup:start_link({local, myer_sup},
                            {
                              {one_for_one, 10, timer:seconds(5)},
                              [ get_childspec(E) || E <- baseline_app:args(myer,StartArgs) ]
                            }).

stop([]) ->
    void.

%% == internal ==

get_childspec({Name,Args}) ->
    {
      Name,
      {
        baseline_sup,
        start_link,
        [
         {
           {simple_one_for_one, 10, timer:seconds(5)},
           [
            {
              undefined,
              {
                myer_client,
                start_link,
                [
                 update(Args)
                ]
              },
              temporary,
              timer:seconds(5),
              worker,
              [gen_server]
            }
           ]
         }
        ]
      },
      permanent,
      timer:seconds(5),
      supervisor,
      []
    }.

update(Args) ->
    L = [
         {address,               fun baseline_lists:get/4,            ["localhost"]},
         {port,                  fun baseline_lists:get_as_integer/4, [3306]},
         {user,                  fun baseline_lists:get_as_binary/4,  [<<"root">>]},
         {password,              fun baseline_lists:get_as_binary/4,  [<<"">>]},
         {database,              fun baseline_lists:get_as_binary/4,  [<<"">>]},
         {default_character_set, fun baseline_lists:get_as_integer/4, [?CHARSET_utf8_general_ci]},
         {compress,              fun baseline_lists:get_as_boolean/4, [false]},
         {autocommit,            fun baseline_lists:get_as_boolean/4, [false]},
         {max_allowed_packet,    fun baseline_lists:get_as_integer/6, [4194304,?MAX_PACKET_LENGTH,?MIN_PACKET_LENGTH]},
         {timeout,               fun baseline_lists:get_as_integer/4, [10]} % != infinity
        ],
    [ {K,apply(F,[K|[1|[Args|A]]])} || {K,F,A} <- L ].
