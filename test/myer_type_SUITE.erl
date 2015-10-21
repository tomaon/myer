%% =============================================================================
%% =============================================================================

-module(myer_type_SUITE).

-include("internal.hrl").

%% -- callback: ct --
-export([all/0,
         groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% -- public --
-export([real_types_test_11_2_1/1,
         real_types_test_11_2_2/1,
         real_types_test_11_2_3/1, real_types_test_11_2_4/1,
         real_types_test_11_3_1/1, real_types_test_11_3_2/1,
         real_types_test_11_3_3/1,
         real_types_test_11_4_1/1,
         real_types_test_11_4_2/1,
         real_types_test_11_4_3/1,
         real_types_test_11_4_4/1, real_types_test_11_4_5/1,
         real_types_test_11_6/1]).
-export([stmt_types_test_11_2_1/1,
         stmt_types_test_11_2_2/1,
         stmt_types_test_11_2_3/1, stmt_types_test_11_2_4/1,
         stmt_types_test_11_3_1/1, stmt_types_test_11_3_2/1,
         stmt_types_test_11_3_3/1,
         stmt_types_test_11_4_1/1,
         stmt_types_test_11_4_2/1,
         stmt_types_test_11_4_3/1,
         stmt_types_test_11_4_4/1, stmt_types_test_11_4_5/1,
         stmt_types_test_11_6/1]).

%% == callback: ct ==

all() -> [
          {group, groups_internal}
         ].

groups() -> [

             {groups_internal, [sequence], [
                                            {group, group_normal}
                                           ]},

             {group_normal, [sequence], [
                                         {group,real_test},
                                         {group,stmt_test}
                                        ]},

             {real_test, [], [
                              real_types_test_11_2_1,
                              real_types_test_11_2_2,
                              real_types_test_11_2_3, real_types_test_11_2_4,
                              real_types_test_11_3_1, real_types_test_11_3_2,
                              real_types_test_11_3_3,
                              real_types_test_11_4_1,
                              real_types_test_11_4_2,
                              real_types_test_11_4_3,
                              real_types_test_11_4_4, real_types_test_11_4_5,
                              real_types_test_11_6
                             ]},

             {stmt_test, [], [
                              stmt_types_test_11_2_1,
                              stmt_types_test_11_2_2,
                              stmt_types_test_11_2_3, stmt_types_test_11_2_4,
                              stmt_types_test_11_3_1, stmt_types_test_11_3_2,
                              stmt_types_test_11_3_3,
                              stmt_types_test_11_4_1,
                              stmt_types_test_11_4_2,
                              stmt_types_test_11_4_3,
                              stmt_types_test_11_4_4, stmt_types_test_11_4_5,
                              stmt_types_test_11_6
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

%% -- real_* --

real_types_test(Config, Table)
  when <<"data_types_11_6">> =:= Table ->
    real_types_test(Config, Table, ?config(version,Config) >= [5,7,0]);
real_types_test(Config, Table) ->
    real_types_test(Config, Table, ?config(version,Config) >= [5,1,0]).

real_types_test(_Config, _Table, false) ->
    {skip, not_supported};
real_types_test(Config, Table, true) ->

    Q = <<"SELECT * FROM ", Table/binary>>,

    F = fields(Config,Table),
    R = rows(Config,Table),

    {ok, F, R, _} = real_query(Config, Q),

    ok.

real_types_test_11_2_1(Config) -> real_types_test(Config, <<"data_types_11_2_1">>).
real_types_test_11_2_2(Config) -> real_types_test(Config, <<"data_types_11_2_2">>).
real_types_test_11_2_3(Config) -> real_types_test(Config, <<"data_types_11_2_3">>).
real_types_test_11_2_4(Config) -> real_types_test(Config, <<"data_types_11_2_4">>).
real_types_test_11_3_1(Config) -> real_types_test(Config, <<"data_types_11_3_1">>).
real_types_test_11_3_2(Config) -> real_types_test(Config, <<"data_types_11_3_2">>).
real_types_test_11_3_3(Config) -> real_types_test(Config, <<"data_types_11_3_3">>).
real_types_test_11_4_1(Config) -> real_types_test(Config, <<"data_types_11_4_1">>).
real_types_test_11_4_2(Config) -> real_types_test(Config, <<"data_types_11_4_2">>).
real_types_test_11_4_3(Config) -> real_types_test(Config, <<"data_types_11_4_3">>).
real_types_test_11_4_4(Config) -> real_types_test(Config, <<"data_types_11_4_4">>).
real_types_test_11_4_5(Config) -> real_types_test(Config, <<"data_types_11_4_5">>).
real_types_test_11_6(Config) -> real_types_test(Config, <<"data_types_11_6">>).

%%  -- stmt_* --

stmt_types_test(Config, Table)
  when <<"data_types_11_6">> =:= Table ->
    stmt_types_test(Config, Table, ?config(version,Config) >= [5,7,0]);
stmt_types_test(Config, Table) ->
    stmt_types_test(Config, Table, ?config(version,Config) >= [5,1,0]).

stmt_types_test(_Config, _Table, false) ->
    {skip, not_supported};
stmt_types_test(Config, Table, true) ->

    Q = <<"SELECT * FROM ", Table/binary,  " WHERE k > ?">>,

    Name = list_to_binary(?MODULE_STRING),

    {ok, _} = prepare(Config, Name, Q),

    R = rows(Config,Table),

    {ok, _, R, _} = execute(Config, Name, [0]),

    {ok, _} = unprepare(Config, Name).

stmt_types_test_11_2_1(Config) -> stmt_types_test(Config, <<"data_types_11_2_1">>).
stmt_types_test_11_2_2(Config) -> stmt_types_test(Config, <<"data_types_11_2_2">>).
stmt_types_test_11_2_3(Config) -> stmt_types_test(Config, <<"data_types_11_2_3">>).
stmt_types_test_11_2_4(Config) -> stmt_types_test(Config, <<"data_types_11_2_4">>).
stmt_types_test_11_3_1(Config) -> stmt_types_test(Config, <<"data_types_11_3_1">>).
stmt_types_test_11_3_2(Config) -> stmt_types_test(Config, <<"data_types_11_3_2">>).
stmt_types_test_11_3_3(Config) -> stmt_types_test(Config, <<"data_types_11_3_3">>).
stmt_types_test_11_4_1(Config) -> stmt_types_test(Config, <<"data_types_11_4_1">>).
stmt_types_test_11_4_2(Config) -> stmt_types_test(Config, <<"data_types_11_4_2">>).
stmt_types_test_11_4_3(Config) -> stmt_types_test(Config, <<"data_types_11_4_3">>).
stmt_types_test_11_4_4(Config) -> stmt_types_test(Config, <<"data_types_11_4_4">>).
stmt_types_test_11_4_5(Config) -> stmt_types_test(Config, <<"data_types_11_4_5">>).
stmt_types_test_11_6(Config) -> stmt_types_test(Config, <<"data_types_11_6">>).

%% == internal ==

%% -- filed(s) --

field(Table) ->
    #field{catalog = <<"def">>, db = <<"test">>,
           table = Table, org_table = Table, charsetnr = 63}.

fields(_Config, <<"data_types_11_2_1">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0}, % 20483, << 14,12,1,0
     F#field{name = <<"ti">>, org_name = <<"ti">>,
             length = 4, type = 1, flags = 0, decimals = 0},
     F#field{name = <<"si">>, org_name = <<"si">>,
             length = 6, type = 2, flags = 0, decimals = 0},
     F#field{name = <<"mi">>, org_name = <<"mi">>,
             length = 9, type = 9, flags = 0, decimals = 0},
     F#field{name = <<"i">>, org_name = <<"i">>,
             length = 11, type = 3, flags = 0, decimals = 0},
     F#field{name = <<"bi">>, org_name = <<"bi">>,
             length = 20, type = 8, flags = 0, decimals = 0},
     F#field{name = <<"ut">>, org_name = <<"ut">>,
             length = 3, type = 1, flags = 32, decimals = 0}, % 32, << 5
     F#field{name = <<"us">>, org_name = <<"us">>,
             length = 5, type = 2, flags = 32, decimals = 0},
     F#field{name = <<"um">>, org_name = <<"um">>,
             length = 8, type = 9, flags = 32, decimals = 0},
     F#field{name = <<"ui">>, org_name = <<"ui">>,
             length = 10, type = 3, flags = 32, decimals = 0},
     F#field{name = <<"ub">>, org_name = <<"ub">>,
             length = 20, type = 8, flags = 32, decimals = 0},
     F#field{name = <<"ix">>, org_name = <<"ix">>,
             length = 11, type = 3, flags = 0, decimals = 0}
    ];
fields(_Config, <<"data_types_11_2_2">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"d">>, org_name = <<"d">>,
             length = 11, type = 246, flags = 0, decimals = 0},
     F#field{name = <<"ds">>, org_name = <<"ds">>,
             length = 7, type = 246, flags = 0, decimals = 2},
     F#field{name = <<"dm">>, org_name = <<"dm">>,
             length = 66, type = 246, flags = 0, decimals = 0},
     F#field{name = <<"dd">>, org_name = <<"dd">>,
             length = 33, type = 246, flags = 0, decimals = 30},
     F#field{name = <<"de">>, org_name = <<"de">>,
             length = 33, type = 246, flags = 0, decimals = 15},
     F#field{name = <<"n">>, org_name = <<"n">>,
             length = 11, type = 246, flags = 0, decimals = 0},
     F#field{name = <<"ns">>, org_name = <<"ns">>,
             length = 7, type = 246, flags = 0, decimals = 2},
     F#field{name = <<"nm">>, org_name = <<"nm">>,
             length = 66, type = 246, flags = 0, decimals = 0},
     F#field{name = <<"nd">>, org_name = <<"nd">>,
             length = 33, type = 246, flags = 0, decimals = 30}
    ];
fields(_Config, <<"data_types_11_2_3">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"f">>, org_name = <<"f">>,
             length = 12, type = 4, flags = 0, decimals = 31},
     F#field{name = <<"d">>, org_name = <<"d">>,
             length = 22, type = 5, flags = 0, decimals = 31},
     F#field{name = <<"r">>, org_name = <<"r">>,
             length = 22, type = 5, flags = 0, decimals = 31},
     F#field{name = <<"uf">>, org_name = <<"uf">>,
             length = 12, type = 4, flags = 32, decimals = 31},
     F#field{name = <<"ud">>, org_name = <<"ud">>,
             length = 22, type = 5, flags = 32, decimals = 31},
     F#field{name = <<"ur">>, org_name = <<"ur">>,
             length = 22, type = 5, flags = 32, decimals = 31}
    ];
fields(_Config, <<"data_types_11_2_4">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"b">>, org_name = <<"b">>,
             length = 1, type = 16, flags = 32, decimals = 0}, % 32, << 5
     F#field{name = <<"bm">>, org_name = <<"bm">>,
             length = 64, type = 16, flags = 32, decimals = 0}
    ];
fields(Config, <<"data_types_11_3_1">>=B) ->
    F = field(B),
    case ?config(version, Config) of % diff: flags
        Version when Version >= [5,6,0] ->
            [
             F#field{name = <<"k">>, org_name = <<"k">>,
                     length = 11, type = 3, flags = 20483, decimals = 0},
             F#field{name = <<"d">>, org_name = <<"d">>,
                     length = 10, type = 10, flags = 128, decimals = 0}, % 128, << 7
             F#field{name = <<"dt">>, org_name = <<"dt">>,
                     length = 19, type = 12, flags = 128, decimals = 0},
             F#field{name = <<"ts">>, org_name = <<"ts">>,
                     length = 19, type = 7, flags = 128, decimals = 0}
            ];
        Version when Version >= [5,1,0] ->
            [
             F#field{name = <<"k">>, org_name = <<"k">>,
                     length = 11, type = 3, flags = 20483, decimals = 0},
             F#field{name = <<"d">>, org_name = <<"d">>,
                     length = 10, type = 10, flags = 128, decimals = 0}, % 128, << 7
             F#field{name = <<"dt">>, org_name = <<"dt">>,
                     length = 19, type = 12, flags = 128, decimals = 0},
             F#field{name = <<"ts">>, org_name = <<"ts">>,
                     length = 19, type = 7, flags = 224, decimals = 0}  % 224, << 7,6,5
            ]
    end;
fields(Config, <<"data_types_11_3_2">>=B) ->
    F = field(B),
    case ?config(version, Config) of % diff:length
        Version when Version >= [5,6,0] ->
            [
             F#field{name = <<"k">>, org_name = <<"k">>,
                     length = 11, type = 3, flags = 20483, decimals = 0},
             F#field{name = <<"t">>, org_name = <<"t">>,
                     length = 10, type = 11, flags = 128, decimals = 0} % 128, << 7
            ];
        Version when Version >= [5,1,0] ->
            [
             F#field{name = <<"k">>, org_name = <<"k">>,
                     length = 11, type = 3, flags = 20483, decimals = 0},
             F#field{name = <<"t">>, org_name = <<"t">>,
                     length = 8, type = 11, flags = 128, decimals = 0} % 128, << 7
            ]
    end;
fields(_Config, <<"data_types_11_3_3">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"y">>, org_name = <<"y">>,
             length = 4, type = 13, flags = 96, decimals = 0} % 96, << 6,5
    ];
fields(_Config, <<"data_types_11_4_1">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"c">>, org_name = <<"c">>, charsetnr = 33,
             length = 0, type = 254, flags = 0, decimals = 0},
     F#field{name = <<"cf">>, org_name = <<"cf">>, charsetnr = 33,
             length = 15, type = 254, flags = 0, decimals = 0},
     F#field{name = <<"cs">>, org_name = <<"cs">>, charsetnr = 33,
             length = 18, type = 254, flags = 0, decimals = 0},
     F#field{name = <<"cm">>, org_name = <<"cm">>, charsetnr = 33,
             length = 21, type = 254, flags = 0, decimals = 0},
     F#field{name = <<"v">>, org_name = <<"v">>, charsetnr = 33,
             length = 0, type = 253, flags = 0, decimals = 0},
     F#field{name = <<"vf">>, org_name = <<"vf">>, charsetnr = 33,
             length = 15, type = 253, flags = 0, decimals = 0},
     F#field{name = <<"vs">>, org_name = <<"vs">>, charsetnr = 33,
             length = 18, type = 253, flags = 0, decimals = 0},
     F#field{name = <<"vm">>, org_name = <<"vm">>, charsetnr = 33,
             length = 21, type = 253, flags = 0, decimals = 0}
    ];
fields(_Config, <<"data_types_11_4_2">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"b">>, org_name = <<"b">>,
             length = 0, type = 254, flags = 128, decimals = 0}, % 128 << 7
     F#field{name = <<"bf">>, org_name = <<"bf">>,
             length = 5, type = 254, flags = 128, decimals = 0},
     F#field{name = <<"bs">>, org_name = <<"bs">>,
             length = 6, type = 254, flags = 128, decimals = 0},
     F#field{name = <<"bm">>, org_name = <<"bm">>,
             length = 7, type = 254, flags = 128, decimals = 0},
     F#field{name = <<"v">>, org_name = <<"v">>,
             length = 0, type = 253, flags = 128, decimals = 0},
     F#field{name = <<"vf">>, org_name = <<"vf">>,
             length = 5, type = 253, flags = 128, decimals = 0},
     F#field{name = <<"vs">>, org_name = <<"vs">>,
             length = 6, type = 253, flags = 128, decimals = 0},
     F#field{name = <<"vm">>, org_name = <<"vm">>,
             length = 7, type = 253, flags = 128, decimals = 0}
    ];
fields(_Config, <<"data_types_11_4_3">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"tb">>, org_name = <<"tb">>,
             length = 255, type = 252, flags = 144, decimals = 0}, % 144 << 7,4
     F#field{name = <<"b">>, org_name = <<"b">>,
             length = 65535, type = 252, flags = 144, decimals = 0},
     F#field{name = <<"mb">>, org_name = <<"mb">>,
             length = 16777215, type = 252, flags = 144, decimals = 0},
     F#field{name = <<"lb">>, org_name = <<"lb">>,
             length = 4294967295, type = 252, flags = 144, decimals = 0},
     F#field{name = <<"tt">>, org_name = <<"tt">>, charsetnr = 33,
             length = 765, type = 252, flags = 16, decimals = 0},  % 16 << 4
     F#field{name = <<"t">>, org_name = <<"t">>, charsetnr = 33,
             length = 196605, type = 252, flags = 16, decimals = 0},
     F#field{name = <<"mt">>, org_name = <<"mt">>, charsetnr = 33,
             length = 50331645, type = 252, flags = 16, decimals = 0},
     F#field{name = <<"lt">>, org_name = <<"lt">>, charsetnr = 33,
             length = 4294967295, type = 252, flags = 16, decimals = 0}
    ];
fields(_Config, <<"data_types_11_4_4">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"e">>, org_name = <<"e">>, charsetnr = 33,
             length = 21, type = 254, flags = 256, decimals = 0} % 256, << 8
    ];
fields(_Config, <<"data_types_11_4_5">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"s">>, org_name = <<"s">>, charsetnr = 33,
             length = 15, type = 254, flags = 2048, decimals = 0} % 2048, << 11
    ];
fields(_Config, <<"data_types_11_6">>=B) ->
    F = field(B),
    [
     F#field{name = <<"k">>, org_name = <<"k">>,
             length = 11, type = 3, flags = 20483, decimals = 0},
     F#field{name = <<"j">>, org_name = <<"j">>, charsetnr = 63,
             length = 4294967295, type = 245, flags = 144, decimals = 0} % 144, << 7,4
    ];
fields(_Config, _Table) ->
    [].

%% -- rows --

rows(Config, Table) ->
    rows(Config, Table, all).

rows(_Config, <<"data_types_11_2_1">>, all) ->
    [
     [101,0,null,null,null,null,null,null,null,null,null,99],
     [102,-128,null,null,null,null,null,null,null,null,null,99],
     [103,127,null,null,null,null,null,null,null,null,null,99],
     [151,null,null,null,null,null,0,null,null,null,null,99],
     [152,null,null,null,null,null,255,null,null,null,null,99],
     [201,null,0,null,null,null,null,null,null,null,null,99],
     [202,null,-32768,null,null,null,null,null,null,null,null,99],
     [203,null,32767,null,null,null,null,null,null,null,null,99],
     [251,null,null,null,null,null,null,0,null,null,null,99],
     [252,null,null,null,null,null,null,65535,null,null,null,99],
     [301,null,null,0,null,null,null,null,null,null,null,99],
     [302,null,null,-8388608,null,null,null,null,null,null,null,99],
     [303,null,null,8388607,null,null,null,null,null,null,null,99],
     [351,null,null,null,null,null,null,null,0,null,null,99],
     [352,null,null,null,null,null,null,null,16777215,null,null,99],
     [401,null,null,null,0,null,null,null,null,null,null,99],
     [402,null,null,null,-2147483648,null,null,null,null,null,null,99],
     [403,null,null,null,2147483647,null,null,null,null,null,null,99],
     [451,null,null,null,null,null,null,null,null,0,null,99],
     [452,null,null,null,null,null,null,null,null,4294967295,null,99],
     [501,null,null,null,null,0,null,null,null,null,null,99],
     [502,null,null,null,null,-9223372036854775808,null,null,null,null,null,99],
     [503,null,null,null,null,9223372036854775807,null,null,null,null,null,99],
     [551,null,null,null,null,null,null,null,null,null,0,99],
     [552,null,null,null,null,null,null,null,null,null,18446744073709551615,99]
    ];
rows(_Config, <<"data_types_11_2_2">>, all) ->
    [
     [101,0.0,null,null,null,null,null,null,null,null],
     [102,-9999999999.0,null,null,null,null,null,null,null,null],
     [103,9999999999.0,null,null,null,null,null,null,null,null],
     [201,null,0.0,null,null,null,null,null,null,null],
     [202,null,-999.99,null,null,null,null,null,null,null],
     [203,null,999.99,null,null,null,null,null,null,null],
     [301,null,null,0.0,null,null,null,null,null,null],
     [302,null,null,-1.0e65,null,null,null,null,null,null],
     [303,null,null,1.0e65,null,null,null,null,null,null],
     [401,null,null,null,0.0,null,null,null,null,null],
     [402,null,null,null,-10.0,null,null,null,null,null],
     [403,null,null,null,10.0,null,null,null,null,null],
     [501,null,null,null,null,0.0,null,null,null,null],
     [502,null,null,null,null,-99999999999999.9,null,null,null,null],
     [503,null,null,null,null,-999999999999999.9,null,null,null,null],
     [504,null,null,null,null,-1.0e16,null,null,null,null],
     [505,null,null,null,null,-1.0e15,null,null,null,null],
     [506,null,null,null,null,-9.99999999999999,null,null,null,null],
     [507,null,null,null,null,-9.999999999999998,null,null,null,null],
     [508,null,null,null,null,-10.0,null,null,null,null],
     [512,null,null,null,null,99999999999999.9,null,null,null,null],
     [513,null,null,null,null,999999999999999.9,null,null,null,null],
     [514,null,null,null,null,1.0e16,null,null,null,null],
     [515,null,null,null,null,1.0e15,null,null,null,null],
     [516,null,null,null,null,9.99999999999999,null,null,null,null],
     [517,null,null,null,null,9.999999999999998,null,null,null,null],
     [518,null,null,null,null,10.0,null,null,null,null],
     [601,null,null,null,null,null,0.0,null,null,null],
     [602,null,null,null,null,null,-9999999999.0,null,null,null],
     [603,null,null,null,null,null,9999999999.0,null,null,null],
     [701,null,null,null,null,null,null,0.0,null,null],
     [702,null,null,null,null,null,null,-999.99,null,null],
     [703,null,null,null,null,null,null,999.99,null,null],
     [801,null,null,null,null,null,null,null,0.0,null],
     [802,null,null,null,null,null,null,null,-1.0e65,null],
     [803,null,null,null,null,null,null,null,1.0e65,null],
     [804,null,null,null,null,null,null,null,-1.0e65,null],
     [901,null,null,null,null,null,null,null,null,0.0],
     [902,null,null,null,null,null,null,null,null,-10.0],
     [903,null,null,null,null,null,null,null,null,10.0]
    ];
rows(Config, <<"data_types_11_2_3">>, all) ->
    case ?config(version, Config) of
        Version when Version >= [5,5,0] ->
            [
             [101,0.0,null,null,null,null,null],
             [102,-3.40282e38,null,null,null,null,null],
             [103,-1.17549e-38,null,null,null,null,null],
             [104,1.17549e-38,null,null,null,null,null],
             [105,3.40282e38,null,null,null,null,null],
             [201,null,0.0,null,null,null,null],
             [202,null,-1.7976931348623157e308,null,null,null,null],
             [203,null,-2.2250738585072014e-308,null,null,null,null],
             [204,null,2.2250738585072014e-308,null,null,null,null],
             [205,null,1.7976931348623157e308,null,null,null,null],
             [301,null,null,0.0,null,null,null],
             [302,null,null,-1.7976931348623157e308,null,null,null],
             [303,null,null,-2.2250738585072014e-308,null,null,null],
             [304,null,null,2.2250738585072014e-308,null,null,null],
             [305,null,null,1.7976931348623157e308,null,null,null],
             [401,null,null,null,0.0,null,null],
             [404,null,null,null,1.17549e-38,null,null],
             [405,null,null,null,3.40282e38,null,null],
             [501,null,null,null,null,0.0,null],
             [504,null,null,null,null,2.2250738585072014e-308,null],
             [505,null,null,null,null,1.7976931348623157e308,null],
             [601,null,null,null,null,null,0.0],
             [604,null,null,null,null,null,2.2250738585072014e-308],
             [605,null,null,null,null,null,1.7976931348623157e308]
            ];
        Version when Version > [5,1,0] ->
            [
             [101,0.0,null,null,null,null,null],
             [102,-3.40282e38,null,null,null,null,null],
             [103,-1.17549e-38,null,null,null,null,null],
             [104,1.17549e-38,null,null,null,null,null],
             [105,3.40282e38,null,null,null,null,null],
             [201,null,0.0,null,null,null,null],
             [202,null,undefined,null,null,null,null],
             [203,null,-2.2250738585072e-308,null,null,null,null],
             [204,null,2.2250738585072e-308,null,null,null,null],
             [205,null,undefined,null,null,null,null],
             [301,null,null,0.0,null,null,null],
             [302,null,null,undefined,null,null,null],
             [303,null,null,-2.2250738585072e-308,null,null,null],
             [304,null,null,2.2250738585072e-308,null,null,null],
             [305,null,null,undefined,null,null,null],
             [401,null,null,null,0.0,null,null],
             [404,null,null,null,1.17549e-38,null,null],
             [405,null,null,null,3.40282e38,null,null],
             [501,null,null,null,null,0.0,null],
             [504,null,null,null,null,2.2250738585072e-308,null],
             [505,null,null,null,null,undefined,null],
             [601,null,null,null,null,null,0.0],
             [604,null,null,null,null,null,2.2250738585072e-308],
             [605,null,null,null,null,null,undefined]
            ]
    end;
rows(_Config, <<"data_types_11_2_4">>, all) ->
    [
     [101,0,null],
     [102,1,null],
     [201,null,0],
     [202,null,1],
     [203,null,8],
     [204,null,16],
     [205,null,128],
     [206,null,256],
     [207,null,4096],
     [208,null,9223372036854775808]
    ];
rows(_Config, <<"data_types_11_3_1">>, all) ->
    [
     [101,{2012,5,10},null,null],
     [201,null,{{2012,5,10},{12,34,56}},null],
     [301,null,null,{{2012,5,10},{12,34,56}}]
    ];
rows(_Config, <<"data_types_11_3_2">>, all) ->
    [
     [101,{12,34,56}]
    ];
rows(_Config, <<"data_types_11_3_3">>, all) ->
    [
     [101,2012]
    ];
rows(_Config, <<"data_types_11_4_1">>, all) ->
    JP_test = <<227,129,166,227,129,153,227,129,168>>,
    [
     [101,<<>>,null,null,null,null,null,null,null],
     [102,<<>>,null,null,null,null,null,null,null],
     [201,null,<<"abcde">>,null,null,null,null,null,null],
     [202,null,JP_test,null,null,null,null,null,null],
     [301,null,null,<<"abcdef">>,null,null,null,null,null],
     [302,null,null,JP_test,null,null,null,null,null],
     [401,null,null,null,<<"abcdef">>,null,null,null,null],
     [402,null,null,null,JP_test,null,null,null,null],
     [501,null,null,null,null,<<>>,null,null,null],
     [502,null,null,null,null,<<>>,null,null,null],
     [601,null,null,null,null,null,<<"abcde">>,null,null],
     [602,null,null,null,null,null,JP_test,null,null],
     [701,null,null,null,null,null,null,<<"abcdef">>,null],
     [702,null,null,null,null,null,null,JP_test,null],
     [801,null,null,null,null,null,null,null,<<"abcdef">>],
     [802,null,null,null,null,null,null,null,JP_test]
    ];
rows(_Config, <<"data_types_11_4_2">>, all) ->
    [
     [101,<<>>,null,null,null,null,null,null,null],
     [201,null,<<"abcde">>,null,null,null,null,null,null],
     [301,null,null,<<"abcdef">>,null,null,null,null,null],
     [401,null,null,null,<<97,98,99,100,101,102,0>>,null,null,null,null],
     [501,null,null,null,null,<<>>,null,null,null],
     [601,null,null,null,null,null,<<"abcde">>,null,null],
     [701,null,null,null,null,null,null,<<"abcdef">>,null],
     [801,null,null,null,null,null,null,null,<<"abcdef">>]
    ];
rows(_Config, <<"data_types_11_4_3">>, all) ->
    [
     [101,<<"ABCDEF">>,null,null,null,null,null,null,null],
     [201,null,<<"ABCDEF">>,null,null,null,null,null,null],
     [301,null,null,<<"ABCDEF">>,null,null,null,null,null],
     [401,null,null,null,<<"ABCDEF">>,null,null,null,null],
     [501,null,null,null,null,<<"ABCDEF">>,null,null,null],
     [601,null,null,null,null,null,<<"ABCDEF">>,null,null],
     [701,null,null,null,null,null,null,<<"ABCDEF">>,null],
     [801,null,null,null,null,null,null,null,<<"ABCDEF">>]
    ];
rows(_Config, <<"data_types_11_4_4">>, all) ->
    [
     [101,<<"small">>],
     [102,<<"large">>]
    ];
rows(_Config, <<"data_types_11_4_5">>, all) ->
    [
     [101,<<"S">>],
     [102,<<"L">>]
    ];
rows(_Config, <<"data_types_11_6">>, all) ->
    [
     [101,<<"{}">>],
     [102,<<"{\"k\": 102, \"v\": \"102\"}">>]
    ];
rows(_Config, _Table, _Cond) ->
    [].


call(Config, Function, Args) -> call(Config, myer, Function, Args).
call(Config, Module, Function, Args) -> test(Module, Function, [?config(handle,Config)|Args]).
test(Module, Function, Args) -> baseline_ct:test(Module, Function, Args).

execute(Config, Name, Params) -> call(Config, execute, [Name,Params]).
prepare(Config, Name, Query) -> call(Config, prepare, [Name,Query]).
real_query(Config, Query) -> call(Config, real_query, [Query]).
unprepare(Config, Name) -> call(Config, unprepare, [Name]).
