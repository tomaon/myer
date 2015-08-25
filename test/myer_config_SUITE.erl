%% =============================================================================
%% =============================================================================

-module(myer_config_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0,
         groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2]).

%% -- public --
-export([version_test/1]).
-export([auth_test_pwd/1,
         auth_test_nopwd/1,
         auth_test_oldpwd/1,
         auth_test_oldnopwd/1,
         auth_test_pwderr/1,
         auth_test_nodb/1]).
-export([conf_test_address_atom/1, conf_test_address_list/1, conf_test_address_err/1,
         conf_test_port_err/1,
         conf_test_user_atom/1, conf_test_user_list/1, conf_test_user_err/1,
         conf_test_password_atom/1, conf_test_password_list1/1,
         conf_test_password_list2/1, conf_test_password_err/1,
         conf_test_database_atom/1, conf_test_database_list1/1,
         conf_test_database_list2/1, conf_test_database_err/1,
         conf_test_default_character_set_integer/1,
         conf_test_default_character_set_err/1,
         conf_test_compress_boolean/1, conf_test_compress_err/1,
         conf_test_max_allowed_packet_integer/1,
         conf_test_max_allowed_packet_err/1,
         conf_test_timeout_integer/1, conf_test_timeout_err/1,
         conf_test_other/1]).

%% == callback: ct ==

all() -> [
          version_test,
          {group, groups_internal}
         ].

groups() -> [
             {groups_internal, [sequence], [
                                            {group, group_normal}
                                           ]},

             {group_normal, [sequence], [
                                         {group, auth_test},
                                         {group, conf_test}
                                        ]},

             {auth_test, [], [
                              auth_test_pwd,
                              %% auth_test_nopwd
                              auth_test_oldpwd,
                              %% auth_test_oldnopwd
                              %% auth_test_pwderr
                              auth_test_nodb
                             ]},

             {conf_test, [], [
                              conf_test_address_atom, conf_test_address_list, conf_test_address_err,
                              conf_test_port_err,
                              conf_test_user_atom, conf_test_user_list, conf_test_user_err,
                              conf_test_password_atom, conf_test_password_list1,
                              conf_test_password_list2, conf_test_password_err,
                              conf_test_database_atom, conf_test_database_list1,
                              conf_test_database_list2, conf_test_database_err,
                              conf_test_default_character_set_integer,
                              conf_test_default_character_set_err,
                              conf_test_compress_boolean, conf_test_compress_err,
                              conf_test_max_allowed_packet_integer,
                              conf_test_max_allowed_packet_err,
                              conf_test_timeout_integer, conf_test_timeout_err,
                              conf_test_other
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

init_per_testcase(version_test, Config) ->
    Config;
init_per_testcase(_Testcase, Config) ->
    ok = set_env(?config(env,Config)),
    Config.

%% == public ==

version_test(Config) -> myer_public_SUITE:version_test(Config).

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
    try test(Config, L)
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

%% -- conf_* --

conf_test_address_atom(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {address, ct:get_config(hostname)}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_address_list(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {address, atom_to_list(ct:get_config(hostname))}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_address_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {address, <<>>}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_port_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {port, nobody}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_user_atom(Config) ->
    L = [
         {user, test},
         {password, <<"test">>}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_user_list(Config) ->
    L = [
         {user, "test"},
         {password, <<"test">>}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_user_err(Config) ->
    L = [
         {user, nobody},
         {password, <<"test">>}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_password_atom(Config) ->
    L = [
         {user, <<"test">>},
         {password, test}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_password_list1(Config) ->
    L = [
         {user, <<"test">>},
         {password, "test"}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_password_list2(Config) ->
    L = [
         {user, <<"test_nopwd">>},
         {password, ""}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_password_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, nobody}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_database_atom(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, binary_to_atom(get_env(database),latin1)}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_database_list1(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, binary_to_list(get_env(database))}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_database_list2(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, ""}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_database_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, nowhere}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_default_character_set_integer(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {default_character_set, ?CHARSET_utf8_bin}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_default_character_set_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {default_character_set, false}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_compress_boolean(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {compress, true}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_compress_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {compress, "false"}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_max_allowed_packet_integer(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {max_allowed_packet, 4096}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_max_allowed_packet_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {max_allowed_packet, nobody}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_timeout_integer(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {timeout, 10}
        ],
    try test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_timeout_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {timeout, infinity}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_other(Config) ->
    L = [
         {nobody, false}
        ],
    try test(Config, L)
    catch
        _:_ ->
            ok
    end.

%% == internal ==

get_env(Key) ->
    {ok, L} = test(application, get_env, [myer,mysql_pool]),
    test(proplists, get_value, [Key,L]).

set_env(List) ->
    baseline_ct:set_env(List).

set_env(Config, List) ->
    ok = test(application, set_env, [myer,mysql_pool,List]),
    Config.

test(Config, List) ->
    L = [fun myer_public_SUITE:start_test/1, fun myer_public_SUITE:stop_test/1],
    lists:foldl(fun(E,A) -> E(A) end, set_env(Config,List), L).

test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).
