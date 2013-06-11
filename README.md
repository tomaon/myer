# myer

MySQL driver, using the MySQL network protocol

support: (text|binary)-protocol with compression, blob 

[![Build Status](https://travis-ci.org/tomaon/myer.png)](https://travis-ci.org/tomaon/myer)

## Example

```erlang
% files/shell.config
[
 {myer, [
         {poolboy, [
                    {mysql_pool, % 5.6.12 (source:gcc-4.6.3)
                     [
                      {size, 1},
                      {max_overflow, 3}
                     ],
                     [
                      {address, <<"localhost">>},
                      {port, 20506},
                      {user, <<"test">>},
                      {password, <<"test">>},
                      {database, <<"test">>},
                      {compress, false},
                      {max_allowed_packet, 4194304},
                      {timeout, 10} 
                     ]}
                   ]}
        ]}
].
```

```erl-sh
$ make shell
Erlang R16B (erts-5.10.1) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.1  (abort with ^G)
1> myer:start().
ok
2> {ok, Pid} = myer:connect(mysql_pool).
{ok,<0.45.0>}
3> myer:real_query(Pid, <<"SELECT @@version">>).
{ok,[{field,<<"def">>,<<>>,<<>>,<<>>,<<"@@version">>,<<>>,
            33,30,253,0,31}],
    [[<<"5.6.12-log">>]],
    {result,undefined,undefined,2,0,undefined}}
4> myer:close(mysql_pool, Pid).
ok
5> myer:stop().
ok
```

## Build

``` sh
$ make build
```
