%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(main).

-export([main/4]).

-include("types.hrl").

-spec(main(string(), string(), string(), property_list()) -> any()).
main(SourceFile, PolygonFile, OutputFile, Options) ->
    Polygon = polygon_compiler:compile_polygon(PolygonFile, Options),
    init_work_query(Polygon, OutputFile, Options),

    process_file(SourceFile, Options).



-spec(init_work_query(polygon_function(), string(), property_list()) -> any()).   
init_work_query(Polygon, OutputFile, _Options) ->
    osm_processor:start_link(Polygon),
    osm_writer:start_link(OutputFile).

-spec(process_file(string(), property_list()) -> ok).   
process_file(SourceFile, _Options) ->
    osm_parser:parse(SourceFile).

