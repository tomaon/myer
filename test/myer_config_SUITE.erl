%% =============================================================================
%% =============================================================================

-module(myer_config_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0,
         groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2]).

%% -- public --
-export([auth_test_pwd/1,
         auth_test_nopwd/1,
         auth_test_oldpwd/1,
         auth_test_oldnopwd/1,
         auth_test_pwderr/1,
         auth_test_nodb/1]).

%% == callback: ct ==

all() -> [
          {group, groups_internal}
         ].

groups() -> [
             {groups_internal, [sequence], [
                                            {group, group_normal}
                                           ]},

             {group_normal, [sequence], [
                                         {group, auth_test}
                                        ]},

             {auth_test, [], [
                              auth_test_pwd,
                              %% auth_test_nopwd
                              auth_test_oldpwd,
                              %% auth_test_oldnopwd
                              %% auth_test_pwderr
                              auth_test_nodb
                             ]}
            ].

init_per_group(Group, Config) ->
    case ct:get_config(Group) of
        undefined ->
            Config;
        List ->
            [{env,List}|Config]
    end.

end_per_group(_Group, Config) ->
    proplists:delete(env, Config).

init_per_testcase(_Testcase, Config) ->
    ok = set_env(?config(env,Config)),
    Config.

%% == public ==

%% -- auth_* --

auth_test_pwd(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_nopwd(Config) ->
    L = [
         {user, <<"test_nopwd">>},
         {password, <<>>}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_oldpwd(Config) ->
    L = [
         {user, <<"test_oldpwd">>},
         {password, <<"test">>}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_oldnopwd(Config) ->
    L = [
         {user, <<"test_oldnopwd">>},
         {password, <<>>}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_pwderr(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<>>}
        ],
    try test(Config, L) of
        _ ->
            ct:fail(ebadarg)
    catch
        _:_ ->
            ok
    end.

auth_test_nodb(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, <<>>}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

%% == internal ==

set_env(List) ->
    baseline_ct:set_env(List).

set_env(Config, List) ->
    ok = test(application, set_env, [myer,mysql_pool,List]),
    Config.

test(Config, List) ->
    L = [fun myer_public_SUITE:start_test/1, fun myer_public_SUITE:stop_test/1],
    lists:foldl(fun(E,A) -> E(A) end, set_env(Config,List), L).

test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).
