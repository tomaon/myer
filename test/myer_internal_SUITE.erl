%% =============================================================================
%% =============================================================================

-module(myer_internal_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0,
         groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% -- public --
-export([cover_myer_client_cast/1,
         cover_myer_client_info1/1, cover_myer_client_info2/1]).

%% == callback: ct ==

all() -> [
          {group, groups_internal}
         ].

groups() -> [

             {groups_internal, [sequence], [
                                            {group, group_normal}
                                           ]},

             {group_normal, [sequence], [
                                         cover_myer_client_cast,
                                         cover_myer_client_info1,
                                         cover_myer_client_info2  % TODO, @see tmp/x1
                                        ]}
            ].

init_per_group(Group, Config) ->
    myer_public_SUITE:init_per_group(Group, Config).

end_per_group(Group, Config) ->
    myer_public_SUITE:end_per_group(Group, Config).

init_per_testcase(TestCase, Config) ->
    myer_public_SUITE:init_per_testcase(TestCase, Config).

end_per_testcase(_TestCase, Config) ->
    Config. % != myer_public_SUITE:end_per_testcase/2

%% == public ==

cover_myer_client_cast(Config) ->

    Pid = element(3, ?config(handle,Config)),
    unlink(Pid),

    ok = test(gen_server, cast, [Pid,?MODULE]),

    ok = ct:sleep({seconds, 1}),

    false = is_process_alive(Pid).

cover_myer_client_info1(Config) ->

    Pid = element(3, ?config(handle,Config)),
    unlink(Pid),

    Protocol = element(4, sys:get_state(Pid)),
    true = test(erlang, is_record, [Protocol,protocol]),

    Handle = element(2, Protocol),
    handle = test(erlang, element, [1,Handle]),

    Socket = element(2, Handle),
    socket = test(erlang, element, [1,Socket]),

    Port = element(2, Socket),
    true = test(erlang, is_port, [Port]),

    Pid ! {tcp_closed,Port}, % ...

    ok = ct:sleep({seconds, 1}),

    false = test(erlang, is_process_alive, [Pid]).

cover_myer_client_info2(Config) ->

    Pid = element(3, ?config(handle,Config)),
    unlink(Pid),

    Protocol = element(4, sys:get_state(Pid)),
    true = test(erlang, is_record, [Protocol,protocol]),

    Handle = element(2, Protocol),
    handle = test(erlang, element, [1,Handle]),

    Socket = element(2, Handle),
    socket = test(erlang, element, [1,Socket]),

    Port = element(2, Socket),
    true = test(erlang, is_port, [Port]),

    Port ! {Pid,close},

    ok = ct:sleep({seconds, 1}),

    false = is_process_alive(Pid).

%% == internal ==

test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).
