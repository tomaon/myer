%% =============================================================================
%% =============================================================================

-module(myer_private_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0,
         groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% -- public --
-export([version_test/1]).
-export([cover_myer_client/1]).

%% == callback: ct ==

all() -> [
          version_test,
          {group, groups_private}
         ].

groups() -> [

             {groups_private, [sequence], [
                                           {group, group_normal}
                                          ]},

             {group_normal, [sequence], [
                                         cover_myer_client
                                        ]}
            ].

init_per_group(Group, Config) ->
    myer_public_SUITE:init_per_group(Group, Config).

end_per_group(Group, Config) ->
    myer_public_SUITE:end_per_group(Group, Config).

init_per_testcase(TestCase, Config) ->
    myer_public_SUITE:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    myer_public_SUITE:end_per_testcase(TestCase, Config).

%% == public ==

version_test(Config) -> myer_public_SUITE:version_test(Config).


cover_myer_client(Config) ->

    Handle = ?config(handle, Config),

    Pid = element(3, Handle),

    {error, badarg} = test(myer_client, call, [Pid,?MODULE]),

    ok = test(myer_client, cast, [Pid,?MODULE]),

    Pid ! ?MODULE,

    ok.

%% == internal ==

test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).
