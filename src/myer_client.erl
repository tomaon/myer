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

%% -- internal --
-record(state, {
          module    :: module(),                % myer_protocol
          args      :: properties(),
          handle    :: tuple(),                 % myer_porotocol:handle
          caps      :: non_neg_integer(),
          maxlength :: non_neg_integer(),
          version   :: [integer()]              % TODO, non_neg_integer
         }).

%% == public ==

-spec start_link(properties()) -> {ok,pid()}|ignore|{error,_}.
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
handle_info({Pid,version}, #state{version=V}=S) ->
    Pid ! {self(), V},
    {noreply, S};
handle_info({tcp_closed,_Socket}, #state{}=S) ->
    {stop, tcp_closed, S#state{handle = undefined}};
handle_info({'EXIT',Socket,normal}, #state{}=S)
  when is_port(Socket) ->
    {stop, port_closed, S#state{handle = undefined}};
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

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
    {ok, S#state{module = myer_protocol, args = Args}, 0}.


initialized(#state{module=M,args=A,handle=undefined}=S) ->
    L = [
         get(address, A),
         get(port, A),
         X = get(max_allowed_packet, A),
         get(timeout, A)
        ],
    case apply(M, connect, [L]) of
        {ok, Handle, Handshake} ->
            connected(S#state{handle = Handle, maxlength = X}, Handshake);
        {error, Reason} ->
            {error, Reason, S};
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end.

connected(#state{module=M,args=A,handle=H}=S, #handshake{caps=C,version=V}=R) ->
    L = [
         H,
         get(user, A),
         get(password, A),
         get(database, A),
         R#handshake{
           caps = X = C band myer_protocol:default_caps(get(compress,A)),
           charset = get(default_character_set,A)
}
        ],
    case apply(M, auth, L) of
        {ok, _Result, Handle} ->
            authorized(S#state{handle = Handle, caps = X, version = V});
        {error, Reason, Handle} ->
            {error, Reason, S#state{handle = Handle}}
    end.

authorized(#state{}=S) ->
    {noreply, S#state{args = undefined}}.


ready(Func, Args, #state{module=M,handle=H}=S) ->
    case apply(M, Func, [H|Args]) of
        {ok, Handle} ->
            {reply, ok, S#state{handle = Handle}};
        {ok, Term, Handle} ->
            {reply, {ok,Term}, S#state{handle = Handle}};
        {ok, Term1, Term2, Term3, Handle} ->
            {reply, {ok,Term1,Term2,Term3}, S#state{handle = Handle}};
        {error, Reason, Handle} ->
            {reply, {error,Reason}, S#state{handle = Handle}}
    end.

%% == internal ==

get(Key, List) ->
    baseline_lists:get(Key, 1, List, undefined).
