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
     check_non_convex_polygon,
     check_osm_poly].

check_polygon(CheckFun, List) ->
    F1 = osm_polygon_compiler:compile([{include, List}]),
    CheckFun(F1),
    F2 = osm_polygon_compiler:compile([{include, lists:reverse(List)}]),
    CheckFun(F2),
    ok.

%% Test cases starts here.
%%--------------------------------------------------------------------
check_bounding_box() ->
    [{doc, "Checks poligon by bounding box"}].

check_bounding_box(Config) when is_list(Config) ->
    check_polygon(fun(Fun) ->
                          true = Fun(5, 5),
                          true = Fun(0, 0),
                          true = Fun(10, 5),
                          
                          false = Fun(-5, 0),
                          false = Fun(15, 5),
                          false = Fun(5, -5),
                          false = Fun(5, 15)
                  end, [{0, 0}, {10, 0}, {10, 10}, {0, 10}]).


check_convex_polygon() ->
    [{doc, "Checks convex poligon"}].

check_convex_polygon(Config) when is_list(Config) ->
    check_polygon(fun(Fun) ->
                          true = Fun(0, 0),
                          true = Fun(5, 5),
                          true = Fun(3, 3),
                          false = Fun(0, 10),
                          false = Fun(3, 7)
                  end, [{0, 0}, {10, 0}, {10, 10}]).

check_non_convex_polygon() ->
    [{doc, "Checks convex poligon"}].

check_non_convex_polygon(Config) when is_list(Config) ->
    check_polygon(fun(Fun) ->
                          true = Fun(0, 0), % on
                          true = Fun(0, 5), % on vertex
                          true = Fun(1, 3), % inside
                          false = Fun(1, 5),
                          false = Fun(10, 7)
                  end, [{-5, 0}, {5, 0}, {-5, 10}, {5, 10}]).

check_osm_poly() ->
    [{doc, "Checks convex poligon"}].

check_osm_poly(Config) when is_list(Config) ->
    check_polygon(fun(Fun) ->
                          true = Fun(0, 0), % on
                          true = Fun(5, 0), % on 
                          true = Fun(10, 5), % on
                          false = Fun(10, 10),
                          false = Fun(0, 10),
                          false = Fun(9, 0),
                          false = Fun(9.75, 4.9),
                          true = Fun(9.8, 4.9),
                          true = Fun(9.85, 4.9),
                          true = Fun(9.9, 4.9),
                          false = Fun(9.95, 4.9),
                          false = Fun(10, 4.9),
                          false = Fun(10, 0),
                          false = Fun(15, 5),
                          true = Fun(0.3, 0.1),
                          false = Fun(0.3, 0.4),
                          true = Fun(5, 0.01),
                          false = Fun(5.1, 0.01),
                          true = Fun(4.7, 0.01)
                  end, [{0, 0}, {5, 0}, {10, 5}]).
