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

-module(myer_client).

-include("internal.hrl").

%% -- public --
-export([start_link/1]).
-export([call/2]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- private --
-record(state, {
          args :: [property()],
          handle :: tuple()
         }).

-define(SOCKET(Handle), element(2,Handle)).     % !CAUTION!

%% == public ==

-spec start_link([property()]) -> {ok,pid()}|ignore|{error,_}.
start_link(Args)
  when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


-spec call(pid(),term()) -> term().
call(Pid, Command)
  when is_pid(Pid) ->
    gen_server:call(Pid, Command).

%% == behaviour: gen_server ==

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({Func,Args}, _From, State)
  when is_atom(Func), is_list(Args)->
    ready(Func, Args, State).

handle_cast(_Request, State) ->
    {stop, enotsup, State}.

handle_info(timeout, State) ->
    initialized(State);
handle_info({tcp_closed,Socket}, #state{handle=H}=S)
  when Socket =:= ?SOCKET(H) ->
    {stop, tcp_closed, S};
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State}.

%% == private ==

cleanup(#state{handle=H}=S)
  when undefined =/= H ->
    _ = myer_protocol:close(H),
    cleanup(S#state{handle = undefined});
cleanup(#state{}) ->
    baseline:flush().

setup(Args) ->
    process_flag(trap_exit, true),
    loaded(Args, #state{}).


loaded(Args, #state{}=S) ->
    {ok, S#state{args = Args}, 0}.


initialized(#state{args=A,handle=undefined}=S) ->
    L = [
         proplists:get_value(address, A, "localhost"),
         proplists:get_value(port, A, 3306),
         proplists:get_value(default_character_set, A, ?CHARSET_utf8_general_ci),
         proplists:get_value(compress, A, false),
         proplists:get_value(max_allowed_packet, A, 4194304),
         timer:seconds(proplists:get_value(timeout, A, 10))
        ],
    case apply(myer_protocol, connect, [L]) of
        {ok, Handle} ->
            connected(S#state{handle = Handle});
        {error, Reason} ->
            {error, Reason, S};
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end.

connected(#state{args=A,handle=H}=S) ->
    L = [
         proplists:get_value(user, A, <<"root">>),
         proplists:get_value(password, A, <<"">>),
         proplists:get_value(database, A, <<"">>)
        ],
    case apply(myer_protocol, auth, [H|L]) of
        {ok, _Result, Handle} ->
            authorized(S#state{handle = Handle});
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end.

authorized(#state{}=S) ->
    {noreply, S}.


ready(Func, Args, #state{handle=H}=S) ->
    case apply(myer_protocol, Func, [H|Args]) of
        {ok, Handle} ->
            {reply, ok, S#state{handle = Handle}};
        {ok, Term, Handle} ->
            {reply, {ok,Term}, S#state{handle = Handle}};
        {ok, Term1, Term2, Term3, Handle} ->
            {reply, {ok,Term1,Term2,Term3}, S#state{handle = Handle}};
        {error, Reason, Handle} ->
            {reply, {error,Reason}, S#state{handle = Handle}}
    end.
