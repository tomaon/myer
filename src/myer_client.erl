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

-module(myer_client).

-include("myer_internal.hrl").

%% -- public --
-export([start_link/1, start_link/2, stop/1]).
-export([call/2, cast/2]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- private --

-record(state, {
          handle :: tuple(),
          auth :: boolean()
         }).

%% == public ==

-spec start_link([property()]) -> {ok,pid()}|ignore|{error,_}.
start_link(Args) ->
    start_link(Args, false). % for poolboy

-spec start_link([property()],boolean()) -> {ok,pid()}|ignore|{error,_}.
start_link(Args, false)
  when is_list(Args) ->
    try lists:foldl(fun validate/2, [], proplists:unfold(Args)) of
        List ->
            start_link(List, true)
    catch
        Reason ->
            {error, Reason}
    end;
start_link(Args, true)
  when is_list(Args) ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Args}, infinity) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    stop(Pid),
                    {error, Reason}
            end;
        Other ->
            Other
    end.

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

-spec call(pid(),term()) -> term().
call(Pid, Command)
  when is_pid(Pid) ->
    gen_server:call(Pid, Command).

-spec cast(pid(),term()) -> ok.
cast(Pid, Command)
  when is_pid(Pid) ->
    gen_server:cast(Pid, Command).

%% == behaviour: gen_server ==

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setup,Args}, _From, #state{}=S) ->
    case setup(Args, S) of
        {ok, State} ->
            {reply, ok, State};
        {error, Reason, State} ->
            {reply, {error,Reason}, State}
    end;
handle_call({Func,Args}, _From, #state{handle=H}=S)
  when is_atom(Func), is_list(Args)->
    case apply(myer_protocol, Func, [H|Args]) of
        {ok, Term, Handle} ->
            {reply, {ok,Term}, S#state{handle = Handle}};
        {error, Reason, Handle} ->
            {reply, {error,Reason}, S#state{handle = Handle}}
    end;
handle_call(Request, From, State) ->
    io:format("~p [~p:handle_call] req=~p,~p~n", [self(),?MODULE,Request,From]),
    {reply, {error,badarg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    io:format("~p [~p:handle_cast] req=~p~n", [self(),?MODULE,Request]),
    {noreply, State}.

handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    io:format("~p [~p:handle_info] info=~p~n", [self(),?MODULE,Info]),
    {noreply, State}.

%% == private: state ==

cleanup(#state{handle=H}=S)
  when undefined =/= H ->
    _ = myer_protocol:close(H),
    cleanup(S#state{handle = undefined, auth = false});
cleanup(#state{}) ->
    %%io:format("~p [~p:cleanup]~n",[self(),?MODULE]),
    flush().

setup([]) ->
    %%io:format("~p [~p:setup]~n",[self(),?MODULE]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

setup(Args, #state{handle=undefined}=S) ->
    L = [
         proplists:get_value(address, Args, "localhost"),
         proplists:get_value(port, Args, 3306),
         proplists:get_value(default_character_set, Args, ?CHARSET_utf8_general_ci),
         proplists:get_value(compress, Args, false),
         proplists:get_value(max_allowed_packet, Args, 4194304),
         timer:seconds(proplists:get_value(timeout, Args, 10))
        ],
    case apply(myer_protocol, connect, [L]) of
        {ok, undefined, Handle} ->
            setup(Args, S#state{handle = Handle, auth = false});
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end;
setup(Args, #state{handle=H,auth=false}=S) ->
    L = [
         proplists:get_value(user, Args, <<"root">>),
         proplists:get_value(password, Args, <<"">>),
         proplists:get_value(database, Args, <<"">>)
        ],
    case apply(myer_protocol, auth, [H|L]) of
        {ok, _Result, Handle} ->
            setup(Args, S#state{handle = Handle, auth = true});
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end;
setup(_Args, #state{}=S) ->
    {ok, S}.

%% == private ==

flush() ->
    receive _ -> flush() after 0 -> ok end.

validate({address=K,Value}, List) -> % inet:ip_address()|inet:hostname()
    T = if is_atom(Value)                    -> {K, Value};
           is_list(Value), 0 < length(Value) -> {K, Value};
           is_binary(Value), 0 < size(Value) -> {K, binary_to_list(Value)};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({port=K,Value}, List) -> % inet:port_number()
    T = if is_integer(Value)                 -> {K, Value};
           is_list(Value), 0 < length(Value) -> {K, list_to_integer(Value)};
           is_binary(Value), 0 < size(Value) -> {K, binary_to_integer(Value)};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({user=K,Value}, List) -> % binary()
    T = if is_list(Value), 0 < length(Value) -> {K, list_to_binary(Value)};
           is_binary(Value), 0 < size(Value) -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({password=K,Value}, List) -> % binary()
    T = if is_list(Value), 0 < length(Value) -> {K, list_to_binary(Value)};
           is_list(Value)                    -> {K, <<>>};
           is_binary(Value), 0 < size(Value) -> {K, Value};
           is_binary(Value)                  -> {K, <<>>};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({database=K,Value}, List) -> % binary()
    T = if is_list(Value), 0 < length(Value) -> {K, list_to_binary(Value)};
           is_list(Value)                    -> {K, <<>>};
           is_binary(Value), 0 < size(Value) -> {K, Value};
           is_binary(Value)                  -> {K, <<>>};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({default_character_set=K,Value}, List) -> % integer()
    T = if is_integer(Value)                 -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({compress=K,Value}, List) -> % boolean()
    T = if is_boolean(Value)                 -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({max_allowed_packet=K,Value}, List) -> % integer()
    T = if is_integer(Value)                 -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({timeout=K,Value}, List) -> % integer()
    T = if is_integer(Value)                 -> {K, Value};
           true -> throw({badarg,K})
        end,
    [T|List];
validate({Key,_Value}, _List) ->
    throw({badarg,Key}).
