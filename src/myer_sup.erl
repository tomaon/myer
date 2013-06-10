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

-module(myer_sup).

%% -- public --
-export([start_link/2, stop/1]).
-export([find/2]).

%% -- behaviour: supervisor --
-behaviour(supervisor).
-export([init/1]).

-type startlink_ret() :: {ok,pid()}|ignore|{error,_}.
-type sup_name() :: {local,atom()}|{global,atom()}.
-type sup_ref() :: atom()|{atom(),node()}|{global,atom()}|pid().

%% -- private --
-type stop_ret() :: ok.

%% == public ==

-spec start_link(sup_name(), term()) -> startlink_ret().
start_link(SupName, Args) ->
    supervisor:start_link(SupName, ?MODULE, Args).

-spec stop(sup_ref()) -> stop_ret().
stop(SupRef) ->
    stop(SupRef, [ element(2,E) || E <- supervisor:which_children(SupRef) ]).

-spec find(sup_ref(),term()) -> node()|undefined. % pid() -> node(), for poolboy
find(SupRef, Id) ->
    case lists:keyfind(Id, 1, supervisor:which_children(SupRef)) of
        {Id, Child, _Type, _Modules} ->
            Child;
        _ ->
            undefined
    end.

%% == behaviour: supervisor ==

init(Args) ->
    {ok, Args}.

%% == private ==

stop(SupRef, [H|T]) ->
    _ = supervisor:terminate_child(SupRef, H),
    stop(SupRef, T);
stop(SupRef, []) when is_pid(SupRef) ->
    true = exit(SupRef, normal),
    ok;
stop(SupRef, []) -> % TODO : CHECK {atom(),node()},{global,atom()}
    case whereis(SupRef) of
        Term when is_pid(Term) ->
            stop(Term)
    end.
