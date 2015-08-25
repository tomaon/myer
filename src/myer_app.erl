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
    try validate(baseline_app:args(myer,StartArgs)) of
        List ->
            baseline_sup:start_link({local, myer_sup},
                                    {
                                      {one_for_one, 10, timer:seconds(5)},
                                      [ get_childspec(E) || E <- List ]
                                    })
    catch
        Reason ->
            {error, Reason}
    end.

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
                 Args
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

validate(Args) ->
    [ {N,lists:foldl(fun validate/2,[],A)} || {N,A} <- Args ].

validate({address=K,Value}, List)            -> % inet:ip_address()|inet:hostname(), TODO
    T = if is_atom(Value)                    -> {K, Value};
           is_list(Value), 0 < length(Value) -> {K, Value};
           is_binary(Value), 0 < size(Value) -> {K, binary_to_list(Value)};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({port=K,Value}, List)               -> % inet:port_number(), TODO
    T = if is_integer(Value)                 -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({user=K,Value}, List)               -> % binary()
    T = if is_list(Value), 0 < length(Value) -> {K, list_to_binary(Value)};
           is_binary(Value), 0 < size(Value) -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({password=K,Value}, List)           -> % binary()
    T = if is_list(Value), 0 < length(Value) -> {K, list_to_binary(Value)};
           is_list(Value)                    -> {K, <<>>};
           is_binary(Value), 0 < size(Value) -> {K, Value};
           is_binary(Value)                  -> {K, <<>>};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({database=K,Value}, List) ->           % binary()
    T = if is_list(Value), 0 < length(Value) -> {K, list_to_binary(Value)};
           is_list(Value)                    -> {K, <<>>};
           is_binary(Value), 0 < size(Value) -> {K, Value};
           is_binary(Value)                  -> {K, <<>>};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({default_character_set=K,Value}, List) -> % non_neg_integer()
    T = if is_integer(Value), 0 =< Value     -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({compress=K,Value}, List)           -> % boolean()
    T = if is_boolean(Value)                 -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({max_allowed_packet=K,Value}, List) -> % non_neg_integer()
    T = if is_integer(Value), 0 =< Value     -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({timeout=K,Value}, List)            -> % timeout()
    T = if is_integer(Value), 0=< Value      -> {K, Value};
           infinity =:= Value                -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({Key,_Value}, _List) ->
    throw({badarg,Key}).
