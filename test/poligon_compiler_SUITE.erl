%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(poligon_compiler_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [check_bounding_box,
     check_convex_polygon,
     check_non_convex_polygon].

%% Test cases starts here.
%%--------------------------------------------------------------------
check_bounding_box() ->
    [{doc, "Checks poligon by bounding box"}].

check_bounding_box(Config) when is_list(Config) ->
    Fun = polygon_compiler:compile([{include, [{0, 0}, {10, 0}, {10, 10}, {0, 10}]}]),

    true = Fun(5, 5),
    true = Fun(0, 0),
    true = Fun(10, 5),

    false = Fun(-5, 0),
    false = Fun(15, 5),
    false = Fun(5, -5),
    false = Fun(5, 15),
    ok.


check_convex_polygon() ->
    [{doc, "Checks convex poligon"}].

check_convex_polygon(Config) when is_list(Config) ->
    Fun = polygon_compiler:compile([{include, [{0, 0}, {10, 0}, {10, 10}]}]),

    true = Fun(0, 0),
    true = Fun(5, 5),
    true = Fun(3, 3),
    false = Fun(0, 10),
    false = Fun(3, 7),
    
    ok.

check_non_convex_polygon() ->
    [{doc, "Checks convex poligon"}].

check_non_convex_polygon(Config) when is_list(Config) ->
    Fun = polygon_compiler:compile([{include, [{-5, 0}, {5, 0}, {-5, 10}, {5, 10}]}]),

    true = Fun(0, 0),
    true = Fun(0, 5),
    true = Fun(1, 3),
    false = Fun(1, 5),
    false = Fun(10, 7),
    
    ok.
