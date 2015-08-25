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
                                         cover_myer_client_info1, cover_myer_client_info2
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

    Protocol = element(3, sys:get_state(Pid)),
    true = is_record(Protocol, protocol),

    Network = element(2, Protocol),
    handle = element(1, Network),

    Socket = element(2, Network),
    true = is_port(Socket),

    Pid ! {tcp_closed,Socket}, % ...

    ok = ct:sleep({seconds, 1}),

    false = is_process_alive(Pid).

cover_myer_client_info2(Config) ->

    Pid = element(3, ?config(handle,Config)),
    unlink(Pid),

    Protocol = element(3, sys:get_state(Pid)),
    true = is_record(Protocol, protocol),

    Network = element(2, Protocol),
    handle = element(1, Network),

    Socket = element(2, Network),
    true = is_port(Socket),

    Socket ! {Pid,close},

    ok = ct:sleep({seconds, 1}),

    false = is_process_alive(Pid).

%% == internal ==

test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).
