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
          module    :: module(),                % myer_protocol -> myer_socket
          args      :: properties(),
          handle    :: tuple(),                 % myer_porotocol:handle
          caps      :: non_neg_integer(),
          maxlength :: non_neg_integer(),
          version   :: [non_neg_integer()],
          reason    :: reason()
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

handle_call({Func,Args}, _From, #state{reason=undefined}=S)
  when is_atom(Func), is_list(Args)->
    ready(Func, Args, S);
handle_call(_Request, _From, #state{reason=R}=S) -> % stop?, TODO
    {reply, {error,R}, S}.

handle_cast(_Request, State) ->
    {stop, enotsup, State}.

handle_info(timeout, State) ->
    initialized(State);
handle_info({Pid,version}, #state{version=V}=S) ->
    Pid ! {self(), V},
    {noreply, S};
handle_info({tcp_closed,_Socket}, #state{}=S) ->
    {stop, tcp_closed, S#state{handle = undefined}};
%%ndle_info({tcp_error,_Socket}, #state{}=S) ->
%%  {stop, tcp_error, S#state{handle = undefined}};
handle_info({'EXIT',Socket,normal}, #state{}=S)
  when is_port(Socket) ->
    {stop, port_closed, S#state{handle = undefined}};
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{handle=H,caps=C}=S)
  when undefined =/= H ->
    _ = myer_protocol:close(C,H),
    cleanup(S#state{handle = undefined});
cleanup(#state{}) ->
    baseline:flush().

setup(Args) ->
    process_flag(trap_exit, true),
    loaded(Args, #state{}).


loaded(Args, #state{}=S) ->
    {ok, S#state{module = myer_protocol, args = Args}, 0}.


initialized(#state{module=M,args=A,handle=undefined}=S) ->
    case apply(M, connect, [
                            [
                             get(address, A),
                             get(port, A),
                             get(max_allowed_packet, A),
                             get(timeout, A)
                            ]
                           ]) of
        {ok, Handshake, Handle} ->
            connected(S#state{handle = Handle}, Handshake);
        {error, Reason} ->
            interrupted(S#state{reason = Reason});
        {error, Reason, Handle} ->
            interrupted(S#state{handle = Handle, reason = Reason})
    end.

connected(#state{module=M,args=A,handle=X}=S, #handshake{caps=C,maxlength=L,version=V}=H) ->
    case apply(M, auth, [
                         [
                          get(user, A),
                          get(password, A),
                          get(database, A),
                          H#handshake{charset = get(default_character_set,A)},
                          X
                         ]
                        ]) of
        {ok, _Result, Handle} ->
            authorized(S#state{handle = Handle, caps = C, maxlength = L, version = V});
        {error, Reason, Handle} ->
            interrupted(S#state{handle = Handle, caps = C, reason = Reason})
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


interrupted(#state{}=S) ->
    {noreply, S}.

%% == internal ==

get(Key, List) ->
    baseline_lists:get(Key, 1, List, undefined).
