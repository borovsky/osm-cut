%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(polygon_compiler).

-export([compile_polygon/2, compile/1]).
-include("types.hrl").
-include_lib("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% @doc Reads polygon file and creates polygon function
%% @spec compile_polygon(string(), property_list()) -> polygon_function()
%% @end
%%--------------------------------------------------------------------
-spec(compile_polygon(string(), property_list()) -> polygon_function()).
compile_polygon(SourceFile, _Options) ->
    {ok, File} = file:open(SourceFile, [read]),
    io:get_line(File, ""), %Ignore first line
    Polygons = read_polygons(File, []),
    file:close(File),
    polygon_compiler:compile(Polygons).


%%--------------------------------------------------------------------
%% @doc Parse polygon file
%% @spec read_polygons(io_device(), polygon_list()) -> polygon_list()
%% @end
%%--------------------------------------------------------------------
-spec(read_polygons(fd(), polygon_list()) -> polygon_list()).
read_polygons(File, Polygons) ->
    case io:get_line(File, "") of
        eof ->
            erlang:error("Unexpected end of file");
        "END" ++ _ ->
            Polygons;
        Data ->
            case re:run(Data, "(!?)(\\d+)", [{capture, [1, 2], list}]) of
                nomatch ->
                    erlang:error("Incorrect file format: polygon start expected");
                {match, ["", _]} ->
                    read_polygon(File, [{include, []} | Polygons]);
                {match, ["!", _]} ->
                    read_polygon(File, [{exclude, []} | Polygons])
            end
    end.


%%--------------------------------------------------------------------
%% @doc Parse one polygon from file
%% @spec read_polygon(io_device(), polygon_list()) -> polygon_list()
%% @end
%%--------------------------------------------------------------------
-spec(read_polygon(fd(), polygon_list()) -> polygon_list()).
read_polygon(File, [{Type, Points} | OtherPolygons]) ->
    case io:get_line(File, "") of
        eof -> erlang:error("Polygon end not found");
        "END" ++ _ ->
            read_polygons(File, [{Type, Points} | OtherPolygons]);
        Data ->
            read_polygon(File, [{Type, [parsed_points(Data) | Points]} | OtherPolygons])
    end.


%%--------------------------------------------------------------------
%% @doc Adds last point of polygon to begin of point list (if not added)
%% @spec extended_polygon_points(list(point())) -> list(point())
%% @end
%%--------------------------------------------------------------------
-spec(extended_polygon_points(list(point())) -> list(point())).
extended_polygon_points([First | _] = Points) ->
    Last = lists:last(Points),
    case Last of
        First ->
            Points;
        _ ->[Last | Points]
    end.
                     
%%--------------------------------------------------------------------
%% @doc Parse points string
%% @spec parsed_points(string()) -> {float(), float()}
%% @end
%%--------------------------------------------------------------------
-spec(parsed_points(string()) -> {float(), float()}).
parsed_points(String) ->
    FloatRegexp = "\s*(-?\\d+(\\.?\\d+)?([eE][-+]?\\d+)?)\s*",
    Regexp = ["^", FloatRegexp, "\\s+", FloatRegexp, $$],
    case re:run(String, Regexp, [{capture, [1, 4], list}]) of
        {match, [X, Y]} -> {osm_utils:to_float(X), osm_utils:to_float(Y)};
        nomatch ->
            erlang:error("Incorrect point format")
    end.
        
%%--------------------------------------------------------------------
%% @doc Generates function that if point inside polygons
%% @spec compile(polygon_list()) -> polygon_function()
%% @end
%%--------------------------------------------------------------------
-spec(compile(polygon_list()) -> polygon_function()).
compile(Polygons) ->
    IncludePolygons = lists:filter(fun(P) -> element(1, P) == include end, Polygons),
    ExcludePolygons = lists:filter(fun(P) -> element(1, P) == exclude end, Polygons),
    IncludeFuncs = lists:map(fun(Polygon) ->
                                     compile_polygon(Polygon) end, IncludePolygons),
    ExcludeFuncs = lists:map(fun(Polygon) ->
                                     compile_polygon(Polygon) end, ExcludePolygons),
    
    fun(X, Y) ->
            lists:any(fun(Fun) -> Fun(X, Y) end, IncludeFuncs) andalso
                not lists:any(fun(Fun) -> Fun(X, Y) end, ExcludeFuncs)
    end.
                                   
    
%%--------------------------------------------------------------------
%% @doc Generates function that if point inside one polygon
%% @spec compile_polygon(polygon_def()) -> polygon_function()
%% @end
%%--------------------------------------------------------------------
-spec(compile_polygon(polygon_def()) -> polygon_function()).
compile_polygon({_,PrePoints}) ->
    Points = extended_polygon_points(PrePoints),
    io:format("Compiling polygon: ~p~n", [Points]),
    Xs = lists:map(fun({X, _Y}) -> X end, Points),
    Ys = lists:map(fun({_X, Y}) -> Y end, Points),
    Xmin = lists:min(Xs),
    Ymin = lists:min(Ys),
    Xmax = lists:max(Xs),
    Ymax = lists:max(Ys),

    Intervals = points_to_intervals(Points),
    
    fun(X, Y) ->
            if
                X >= Xmin andalso X =< Xmax andalso Y >= Ymin andalso Y =< Ymax ->
                    lists:all(fun(Interval) -> at_right_of(Interval, X, Y) end, Intervals);
                true -> false
            end
    end.

%%--------------------------------------------------------------------
%% @doc Checks if point is at right side of line described by (A*X + B*Y + C = 0)
%% @spec at_right_of(interval(), float(), float()) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(at_right_of(interval(), float(), float()) -> boolean()).
at_right_of({A, B, C}, X, Y) ->
    A * X + B * Y + C >= 0.

%%--------------------------------------------------------------------
%% @doc Converts ordered set of points to list of intervals
%% @spec points_to_intervals(list(point())) -> list(interval())
%% @end
%%--------------------------------------------------------------------
-spec(points_to_intervals(list(point())) -> list(interval())).
points_to_intervals(Points) ->
    [Point1, Point2, {X, Y} | _] = Points,

    % Is poligon specified clockwise
    case at_right_of(interval_for(Point1, Point2), X, Y) of
        true -> points_to_intervals_clockwise(Points);
        false ->
            points_to_intervals_clockwise(lists:reverse(Points))
    end.

%%--------------------------------------------------------------------
%% @doc Converts ordered set of points to list of intervals (points ordered clockwise)
%% @spec points_to_intervals_clockwise(list(point())) -> list(interval())
%% @end
%%--------------------------------------------------------------------
-spec(points_to_intervals_clockwise(list(point())) -> list(interval())).
points_to_intervals_clockwise(Points) ->
    points_to_intervals_clockwise(Points, []).

%%--------------------------------------------------------------------
%% @doc Converts ordered set of points to list of intervals (points ordered clockwise)
%% @spec points_to_intervals_clockwise(list(point()), list(interval())) -> list(interval())
%% @end
%%--------------------------------------------------------------------
-spec(points_to_intervals_clockwise(list(point()), list(interval())) -> list(interval())).
points_to_intervals_clockwise([_Point], Intervals) ->
    Intervals;

points_to_intervals_clockwise([Point1, Point2 | Points], Intervals) ->
    points_to_intervals_clockwise([Point2 | Points], [interval_for(Point1, Point2) | Intervals]).

%%--------------------------------------------------------------------
%% @doc Creates interval from start and end line points
%% @spec interval_for(point(), point()) -> interval()
%% @end
%%--------------------------------------------------------------------
-spec(interval_for(point(), point()) -> interval()).
interval_for({X1, Y1}, {X2, Y2}) ->
    A = Y1 - Y2,
    B = X2 - X1,
    C = -(A * X1 + B * Y1),
    {A, B, C}.
