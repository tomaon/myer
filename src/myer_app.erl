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
                              get_childspecs(args(StartArgs)) % TODO
                            }).

stop([]) ->
    void.

%% == internal ==

args(List) ->
    baseline_app:args(myer, List).

get_childspecs(Args) ->
    [ get_childspec(E) || E <- Args ].

get_childspec({Name,Args})
  when is_atom(Name), is_list(Args) ->
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
                 Args,
                 false
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
