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

-module(myer_config_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("myer/include/myer.hrl").

all() -> [
          {group, test_normal}
         ].

groups() ->
    [
     {test_normal,   [], [
                          {group, test_v56_pool},
                          {group, test_v55_pool},
                          {group, test_v51_pool},
                          {group, test_v50_pool},
                          {group, test_v41_pool},
                          {group, test_v40_pool}
                         ]},

     {test_v56_pool, [], [{group,auth_test}]},
     {test_v55_pool, [], [{group,auth_test}]},
     {test_v51_pool, [], [{group,auth_test}]},
     {test_v50_pool, [], [{group,auth_test}]},
     {test_v41_pool, [], [{group,auth_test}]},
     {test_v40_pool, [], [{group,auth_test},{group,conf_test}]},

     {auth_test, [], [
                      auth_test_pwd,
                      auth_test_nopwd,
                      auth_test_oldpwd,
                      auth_test_oldnopwd,
                      auth_test_pwderr,
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

init_per_suite(Config) ->
    _ = application:load(myer),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(Group, Config) ->
    case list_to_binary(lists:reverse(atom_to_list(Group))) of
        <<"loop_",_/binary>> ->
            [{pool,Group}|Config];
        <<"lamron_",_/binary>> ->
            [{compress,false}|Config];
        _ ->
            Config
    end.

end_per_group(Group, Config) ->
    case list_to_binary(lists:reverse(atom_to_list(Group))) of
        <<"loop_",_/binary>> ->
            proplists:delete(pool, Config);
        <<"lamron_",_/binary>> ->
            proplists:delete(compress, Config);
        _ ->
            Config
    end.

%% == group: auth_test ==

auth_test_pwd(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_nopwd(Config) ->
    L = [
         {user, <<"test_nopwd">>},
         {password, <<>>}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_oldpwd(Config) ->
    L = [
         {user, <<"test_oldpwd">>},
         {password, <<"test">>}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_oldnopwd(Config) ->
    L = [
         {user, <<"test_oldnopwd">>},
         {password, <<>>}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

auth_test_pwderr(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<>>}
        ],
    try do_test(Config, L)
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
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

%% == group: conf_test ==

conf_test_address_atom(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {address, ct:get_config(hostname)}
        ],
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_user_atom(Config) ->
    L = [
         {user, test},
         {password, <<"test">>}
        ],
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_user_list(Config) ->
    L = [
         {user, "test"},
         {password, <<"test">>}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_user_err(Config) ->
    L = [
         {user, nobody},
         {password, <<"test">>}
        ],
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_password_atom(Config) ->
    L = [
         {user, <<"test">>},
         {password, test}
        ],
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_password_list1(Config) ->
    L = [
         {user, <<"test">>},
         {password, "test"}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_password_list2(Config) ->
    L = [
         {user, <<"test_nopwd">>},
         {password, ""}
        ],
    try do_test(Config, L)
    catch
        _:Reason ->
            ct:fail(Reason)
    end.

conf_test_password_err(Config) ->
    L = [
         {user, <<"test">>},
         {password, nobody}
        ],
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_database_atom(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, binary_to_atom(get_value(Config,database),latin1)}
        ],
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_database_list1(Config) ->
    L = [
         {user, <<"test">>},
         {password, <<"test">>},
         {database, binary_to_list(get_value(Config,database))}
        ],
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
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
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

conf_test_other(Config) ->
    L = [
         {nobody, false}
        ],
    try do_test(Config, L)
    catch
        _:_ ->
            ok
    end.

%% -- --

do_test(Config, List) ->
    L = [fun start/1, fun stop/1],
    lists:foldl(fun(E,A) -> E(A) end, set_env(Config,List), L).

get_value(Config, Key) ->
    A = ?config(pool, Config),
    proplists:get_value(Key, ct:get_config(A)).

set_env(Config, List) ->
    A = ?config(pool, Config),
    L1 = [{compress,?config(compress,Config)}|List],
    L2 = lists:foldl(fun proplists:delete/2, ct:get_config(A), proplists:get_keys(L1)),
    ok = application:set_env(myer, poolboy, [{A,[{size,1},{max_overflow,3}],L1 ++ L2}]),
    {ok, L3} = application:get_env(myer, poolboy),
    ct:log("env.poolboy=~p", [L3]),
    Config.

start(Config) ->
    case myer:start() of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.

stop(Config) ->
    case myer:stop() of
        ok ->
            Config;
        {error, Reason} ->
            ct:fail(Reason)
    end.
