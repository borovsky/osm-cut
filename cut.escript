#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose
main([InFile, PolygonFile, OutFile]) ->
    true = code:add_path("ebin"),
    true = code:add_path("lib/erlsom/ebin"),
    osm_cut:main(InFile, PolygonFile, OutFile, [complete_objects]);
main(_) ->
    usage().

usage() ->
    io:format("usage: cut.escript <osm file> <polygon-file> <output-file>~n"),
    halt(1).
