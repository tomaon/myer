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
-export([stat/2, version/2]).
-export([ping/2, refresh/3, select_db/3]).
-export([real_query/3, next_result/2]).
-export([autocommit/3, commit/2, rollback/2]).
-export([prepare/4, unprepare/3, execute/4]).

%% -- behaviour: gen_server --
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-record(state, {
          args   :: properties(),
          handle :: tuple(),                    % myer_handle:handle()
          reason :: reason()
         }).

%% == public ==

-spec start_link(properties()) -> {ok,pid()}|ignore|{error,_}.
start_link(Args)
  when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


-spec stat(pid(),timeout()) -> {ok,binary()}|{error,_}.
stat(Pid, Timeout)
  when is_pid(Pid), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {stat,[]}, Timeout).

-spec version(pid(),timeout()) -> {ok,version()}|{error,_}.
version(Pid, Timeout)
  when is_pid(Pid), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {version,[]}, Timeout).


-spec ping(pid(),timeout()) -> {ok,result()}|{error,_}.
ping(Pid, Timeout)
  when is_pid(Pid), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {ping,[]}, Timeout).

-spec refresh(pid(),integer(),timeout()) -> {ok,result()}|{error,_}.
refresh(Pid, Option, Timeout)
  when is_pid(Pid), is_integer(Option), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {refresh,[Option]}, Timeout).

-spec select_db(pid(),binary(),timeout()) -> {ok,result()}|{error,_}.
select_db(Pid, Database, Timeout)
  when is_pid(Pid), is_binary(Database), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {select_db,[Database]}).


-spec real_query(pid(),binary(),timeout()) ->
                        {ok,result()}|
                        {ok,fields(),rows(),result()}|
                        {error,_}.
real_query(Pid, Query, Timeout)
  when is_pid(Pid), is_binary(Query), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {real_query,[Query]}, Timeout).

-spec next_result(pid(),timeout()) ->
                         {ok,result()}|
                         {ok,fields(),rows(),result()}|
                         {error,_}.
next_result(Pid, Timeout)
  when is_pid(Pid), ?IS_TIMEOUT(Timeout) ->
    gen_server:call(Pid, {next_result,[]}, Timeout).


-spec autocommit(pid(),boolean(),timeout()) -> {ok,result()}|{error,_}.
autocommit(Pid, Bool, Timeout) ->
    gen_server:call(Pid, {autocommit,[Bool]}, Timeout).

-spec commit(pid(),timeout()) -> {ok,result()}|{error,_}.
commit(Pid, Timeout) ->
    real_query(Pid, <<"COMMIT">>, Timeout).

-spec rollback(pid(),timeout()) -> {ok,result()}|{error,_}.
rollback(Pid, Timeout) ->
    real_query(Pid, <<"ROLLBACK">>, Timeout).


-spec prepare(pid(),binary(),binary(),timeout()) -> {ok,result()}|{error,_}.
prepare(Pid, Name, Query, Timeout) ->
    real_query(Pid,  <<"PREPARE ",Name/binary," FROM '",Query/binary,"'">>, Timeout).

-spec unprepare(pid(),binary(),timeout()) -> {ok,result()}|{error,_}.
unprepare(Pid, Name, Timeout) ->
    real_query(Pid, <<"DEALLOCATE PREPARE ",Name/binary>>, Timeout).

-spec execute(pid(),binary(),[term()],timeout()) ->
                     {ok,result()}|
                     {ok,fields(),rows(),result()}|
                     {error,_}.
execute(Pid, Name, Params, Timeout) ->
    {ok, B1, B2} = params_to_binary(Name, 0, Params, [], []),
    case real_query(Pid, <<"SET ",B1/binary,"; EXECUTE ",Name/binary," USING ",B2/binary>>, Timeout) of
        {ok, #result{status=S}} when ?IS_SET(S,?SERVER_MORE_RESULTS_EXISTS) -> % SET
            next_result(Pid, Timeout);                                         % EXECUTE
        {error, Reason} ->
            {error, Reason}
    end.

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
handle_info({tcp_closed,_Socket}, #state{}=S) ->
    {stop, tcp_closed, S#state{handle = undefined}};
handle_info({tcp_error,_Socket}, #state{}=S) ->
    {stop, tcp_error, S#state{handle = undefined}};
handle_info({'EXIT',Socket,normal}, #state{}=S)
  when is_port(Socket) ->
    {stop, port_closed, S#state{handle = undefined}};
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(#state{handle=H}=S)
  when undefined =/= H ->
    _ = myer_protocol:close([H]),
    cleanup(S#state{handle = undefined});
cleanup(#state{}) ->
    baseline:flush().

setup(Args) ->
    process_flag(trap_exit, true),
    loaded(Args, #state{}).


loaded(Args, #state{}=S) ->
    {ok, S#state{args = Args}, 0}.


initialized(#state{args=A}=S) ->
    case myer_protocol:connect([
                                get(address, A),
                                get(port, A),
                                get(max_allowed_packet, A),
                                get(compress, A),
                                get(timeout, A)
                               ]) of
        {ok, Handshake, Handle} ->
            connected(Handshake, S#state{handle = Handle});
        {error, Reason} ->
            interrupted(S#state{reason = Reason});
        {error, Reason, Handle} ->
            interrupted(S#state{handle = Handle, reason = Reason})
    end.

connected(Handshake, #state{args=A,handle=H}=S) ->
    case myer_protocol:auth([
                             H,
                             get(user, A),
                             get(password, A),
                             get(database, A),
                             get(default_character_set, A),
                             Handshake
                            ]) of
        {ok, _Result, Handle} ->
            authorized(S#state{handle = Handle});
        {error, Reason, Handle} ->
            interrupted(S#state{handle = Handle, reason = Reason})
    end.

authorized(#state{args=A,handle=H}=S) ->
    case myer_protocol:autocommit([
                                   H,
                                   get(autocommit, A)
                                  ]) of
        {ok, _Result, Handle} ->
            {noreply, S#state{args = undefined, handle = Handle}};
        {error, Reason, Handle} ->
            interrupted(S#state{handle = Handle, reason = Reason})
    end.


ready(Func, Args, #state{handle=H}=S) ->
    case apply(myer_protocol, Func, [
                                     [H|Args]
                                    ]) of
        {ok, Term, Handle} ->
            {reply, {ok,Term}, S#state{handle = Handle}};
        {ok, Term1, Term2, Term3, Handle} ->
            {reply, {ok,Term1,Term2,Term3}, S#state{handle = Handle}};
        {error, Reason, Handle} ->
            {reply, {error,Reason}, S#state{handle = Handle}}
    end.


interrupted(#state{reason=R}=S) ->
    error_logger:error_msg("interrupted: ~p", [R]),
    {noreply, S}.

%% == internal ==

get(Key, List) ->
    baseline_lists:get(Key, 1, List, undefined).

params_to_binary(_Name, _N, [], [_|T1], [_|T2]) ->
    {ok, list_to_binary(lists:reverse(T1)), list_to_binary(lists:reverse(T2))};
params_to_binary(Name, N, [H|T], L1, L2) ->
    S = integer_to_binary(N),
    K = <<$@, Name/binary, $_, S/binary>>,
    V = to_binary(H),
    params_to_binary(Name, N+1, T, [$,|[<<K/binary,$=,V/binary>>|L1]], [$,|[K|L2]]).

to_binary(null)  ->
    <<"null">>;
to_binary(Term) when is_binary(Term) ->
    <<"'", Term/binary, "'">>;
to_binary(Term) when is_integer(Term) ->
    integer_to_binary(Term, 10);
to_binary(Term) when is_float(Term) ->
    list_to_binary(io_lib:format("~w", [Term]));
to_binary({{Year,Month,Day},{Hour,Minute,Second}}) ->
    list_to_binary(io_lib:fwrite("'~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w'",[Year,Month,Day,Hour,Minute,Second]));
to_binary({bit, Unsigned}) ->
    binary:encode_unsigned(Unsigned, big);
to_binary({date,{Year,Month,Day}}) ->
    list_to_binary(io_lib:fwrite("'~.4.0w-~.2.0w-~.2.0w'",[Year,Month,Day]));
to_binary({time,{Hour,Minute,Second}}) ->
    list_to_binary(io_lib:fwrite("'~.2.0w:~.2.0w:~.2.0w'",[Hour,Minute,Second])).
