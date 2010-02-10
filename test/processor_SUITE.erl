%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(processor_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../include/types.hrl").

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
    [check_simple_process, check_complete_objects_process].

execute_process(OsmFile, PolyFile, Options, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    io:format("~p~n", [DataDir]),
    osm_cut:main(DataDir ++ "osm/" ++ OsmFile,
                 DataDir ++ "poly/"++PolyFile, "",
                 [{writer_module, test_osm_writer}] ++ Options).

assert_exists(Nodes, Tuple) ->
    case lists:any(fun(N) -> is_eq(Tuple, N) end, Nodes) of
        true -> ok;
        false ->
            erlang:error(["Object not found", Tuple, Nodes])
    end.

% Simple node existance check
is_eq({Type, Id}, N) when is_tuple(N) ->
    (Type == element(1, N)) andalso (Id == element(2, N));

% Node element deep check
is_eq(#node{id = Id} = S, #node{id = Id} = D) ->
    case (tuple_to_gen_list(S#node{tags = []}) == tuple_to_gen_list(D#node{tags = []})) andalso
          is_eq_tuples(S#node.tags, D#node.tags) of
        true -> true;
        false -> erlang:error({"Not matched: ", S, D}),
                false
    end;

is_eq(#way{id = Id} = S, #way{id = Id} = D) ->
    case ((tuple_to_gen_list(S#way{tags=[]}) == tuple_to_gen_list(D#way{tags = []})) andalso
          is_eq_tuples(S#way.tags, D#way.tags)) of
        true -> true;
        false -> erlang:error({"Not matched: ", S, D}),
                 false
    end;

is_eq(#relation{id = Id} = S, #relation{id = Id} = D) ->
    case (tuple_to_gen_list(S#relation{tags = [], members = []}) ==
          tuple_to_gen_list(D#relation{tags=[], members = []})) andalso
        is_eq_tuples(S#relation.members, D#relation.members) andalso
        is_eq_tuples(S#relation.tags, D#relation.tags) of
        true -> true;
        false -> erlang:error({"Not matched: ", S, D}),
                 false
    end;

is_eq(Tuple, N) ->
    Tuple == N.

is_eq_tuples(List1, List2) ->
    case erlang:length(List1) == erlang:length(List2) of
        true ->
            is_eq_tuples_int(List1, List2);
        false ->
            erlang:error({"Not matched length:", List1, List2}),
            false
    end.

is_eq_tuples_int([], _) ->
    true;
is_eq_tuples_int([Item | Tail], List) ->
    L = tuple_to_gen_list(Item),
    case lists:any(fun(I) ->
                           L == tuple_to_gen_list(I)
                   end, List) of
        true ->
            is_eq_tuples_int(Tail, List);
        false ->
            erlang:error({"Not matched: ", Item, List}),
            false
    end.                     

tuple_to_gen_list(Tuple) ->
    lists:map(fun(I) -> osm_utils:any_to_bin(I) end, tuple_to_list(Tuple)).

%% Test cases starts here.
%%--------------------------------------------------------------------
check_simple_process() ->
    [{doc, "Checks process simple file"}].

check_simple_process(Config) when is_list(Config) ->
    Nodes = execute_process("1.osm", "simple.poly", [], Config),
    7 = length(Nodes),
    % Check presence of required set of nodes
    assert_exists(Nodes, {node, 1}),
    assert_exists(Nodes, {node, 2}),
    assert_exists(Nodes, {node, 3}),
    assert_exists(Nodes, {way, 1}),
    assert_exists(Nodes, {relation, 1}),

    % Check nodes structure
    assert_exists(Nodes, #node{id = 1, x = 0, y = 0,
                               version = 1,
                               changeset = 440330,
                               user = "smsm1",
                               uid = 6871,
                               timestamp =  "2008-12-17T01:18:42Z",
                              tags = []}),

    assert_exists(Nodes, #way{id = 1, nodes = [1, 2, 3, 1],
                              version = 3,
                              changeset =  1368552,
                              user = "Matt",
                              uid = 70,
                              timestamp = "2009-05-31T13:39:15Z",
                              tags = [
                                      {access, private},
                                      {highway, service}
                                     ]}),
    assert_exists(Nodes, #relation{id = 1,
                                   members = [{way, 1, ""}],
                                   version = 1,
                                   changeset = 3364749,
                                   user = "DSem",
                                   uid = 118927,
                                   timestamp = "2009-12-13T17:06:48Z",
                                   tags = [
                                           {admin_level, 8},
                                           {boundary, administrative},
                                           {name, "Warsaw"},
                                           {type, boundary}
                                          ]}),
    ok.

check_complete_objects_process() ->
    [{doc, "Checks process simple file"}].

check_complete_objects_process(Config) when is_list(Config) ->
    Nodes = execute_process("1.osm", "simple.poly", [complete_objects], Config),
    10 = length(Nodes),
    % Check presence of required set of nodes
    assert_exists(Nodes, {node, 1}),
    assert_exists(Nodes, {node, 2}),
    assert_exists(Nodes, {node, 3}),
    assert_exists(Nodes, {node, 4}),
    assert_exists(Nodes, {way, 1}),
    assert_exists(Nodes, {relation, 1}),
    assert_exists(Nodes, {relation, 2}),
    assert_exists(Nodes, {relation, 4}),

    % Check if way contains all nodes printed
    assert_exists(Nodes, #way{id = 1, nodes = [1, 2, 3, 4, 1],
                              version = 3,
                              changeset =  1368552,
                              user = "Matt",
                              uid = 70,
                              timestamp = "2009-05-31T13:39:15Z",
                              tags = [
                                      {access, private},
                                      {highway, service}
                                     ]}),
    assert_exists(Nodes, #relation{id = 1,
                                   members = [{way, 1, ""}],
                                   version = 1,
                                   changeset = 3364749,
                                   user = "DSem",
                                   uid = 118927,
                                   timestamp = "2009-12-13T17:06:48Z",
                                   tags = [
                                           {admin_level, 8},
                                           {boundary, administrative},
                                           {name, "Warsaw"},
                                           {type, boundary}
                                          ]}),

    assert_exists(Nodes, #relation{id = 2, members = [{node, 4, ""}], tags = []}),
    assert_exists(Nodes, #relation{id = 4, members = [{relation, 2, ""}], tags = []}),
    
    ok.
