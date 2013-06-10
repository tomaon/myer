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

-module(myer_app).

%% -- public --
-export([call/2, call/3]).
-export([checkin/2, checkout/3, deps/0, version/0]).

%% -- behaviour: application --
-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).

%% -- private --
-define(APP, myer).
-define(WORKER, myer_client).

%% == public ==

-spec call(node(),term()) -> any().
call(Pool, Command)
  when is_atom(Pool) ->
    call(Pool, Command, timer:seconds(3)).

-spec call(node(),term(),timeout()) -> any().
call(Pool, Command, Timeout)
  when is_atom(Pool) ->
    case myer_sup:find(myer_sup, Pool) of
        undefined ->
            {error, badarg};
        Pid ->
            F = fun(Worker) -> ?WORKER:call(Worker, Command) end,
            poolboy:transaction(Pid, F, Timeout)
    end.

-spec checkin(atom(),pid()) -> ok|{error,_}.
checkin(Pool, Worker)
  when is_atom(Pool), is_pid(Worker) ->
    case myer_sup:find(myer_sup, Pool) of
        undefined ->
            {error, badarg};
        Child ->
            poolboy:checkin(Child, Worker)
    end.

-spec checkout(atom(),boolean(),timeout()) -> {ok,pid()}|{error,_}.
checkout(Pool, Block, Timeout)
  when is_atom(Pool), is_boolean(Block) ->
    case myer_sup:find(myer_sup, Pool) of
        undefined ->
            {error, badarg};
        Child ->
            case poolboy:checkout(Child, Block, Timeout) of
                full ->
                    {error, full};
                Pid ->
                    {ok, Pid}
            end
    end.

-spec deps() -> [atom()].
deps() ->
    _ = application:load(?APP),
    {ok, List} = application:get_key(?APP, applications),
    lists:foldl(fun proplists:delete/2, List, [kernel,stdlib]).

-spec version() -> [non_neg_integer()].
version() ->
    _ = application:load(?APP),
    {ok, List} = application:get_key(myer, vsn),
    lists:map(fun list_to_integer/1, string:tokens(List, ".")).

%% == behaviour: application ==

-record(state, {
          sup :: pid()
         }).

start(normal, []) ->
    case start_sup(application:get_all_env(?APP)) of
        {ok, Pid} ->
            {ok, Pid, #state{sup = Pid}};
        {error, Reason} ->
            {error, Reason}
    end.

prep_stop(#state{sup=P}=S)
  when undefined =/= P ->
    ok = stop_sup(),
    S#state{sup = undefined}.

stop(#state{sup=undefined}) ->
    ok.

%% == private: sup ==

start_sup(Args) ->
    case lists:foldl(fun setup_sup/2, [], Args) of
        List ->
            T = {one_for_one, 0, timer:seconds(1)},
            myer_sup:start_link({local,myer_sup}, {T,List})
    end.

stop_sup() ->
    myer_sup:stop(myer_sup).

setup_sup({poolboy,Args}, List) ->
    List ++ [ poolboy:child_spec(N,[{worker_module,?WORKER}|O],A) || {N,O,A} <- Args ];
setup_sup({_Key,_Value}, List) ->
    List. % ignore
