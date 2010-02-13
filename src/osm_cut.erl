%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(osm_cut).

-export([main/4]).

-include("types.hrl").

%%--------------------------------------------------------------------
%% @doc OSM processing
%% @spec main(string(), string(), string(), property_list()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(main(string(), string(), string(), property_list()) -> any()).
main(SourceFile, PolygonFile, OutputFile, Options) ->
    Polygon = polygon_compiler:compile_polygon(PolygonFile, Options),

    OsmOptions = [{source_file, SourceFile}, {polygon, Polygon}, {output_file, OutputFile} | Options],
    
    application:set_env(osm, options, OsmOptions),
    ok = application:start(osm),
    Result = process_file(SourceFile, Options),
    application:stop(osm),
    
    Result.


%%--------------------------------------------------------------------
%% @doc Parses file
%% @spec process_file(string(), property_list()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(process_file(string(), property_list()) -> ok).
process_file(SourceFile, Options) ->
    osm_parser:parse(SourceFile),
    WriterModule = proplists:get_value(writer_module, Options, osm_writer),
    WriterModule:processing_result().
