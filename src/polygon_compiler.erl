%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(polygon_compiler).

-export([compile_polygon/2, compile/1]).
-include("types.hrl").

-spec(compile_polygon(string(), property_list()) -> polygon_function()).
compile_polygon(SourceFile, _Options) ->
    {ok, File} = file:open(SourceFile, [read]),
    io:get_line(File, ""), %Ignore first line
    Polygons = read_polygons(File, []),
    file:close(File),
    polygon_compiler:compile(Polygons).


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

read_polygon(File, [{Type, Points} | OtherPolygons] = Polygons) ->
    case io:get_line(File, "") of
        eof -> erlang:error("Polygon end not found");
        "END" ++ _ ->
            read_polygons(File, Polygons);
        Data ->
            read_polygon(File, [{Type, [parsed_points(Data) | Points]} | OtherPolygons])
    end
    .

                     
parsed_points(String) ->
    FloatRegexp = "\s*(-?\\d+(\\.?\\d+)?([eE][-+]?\\d+)?)\s*",
    Regexp = ["^", FloatRegexp, "\\s+", FloatRegexp, $$],
    case re:run(String, Regexp, [{capture, [1, 4], list}]) of
        {match, [X, Y]} -> {erlang:list_to_float(X), erlang:list_to_float(Y)};
        nomatch ->
            erlang:error("Incorrect point format")
    end.
        

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
                                   
    
-spec(compile_polygon(polygon_def()) -> polygon_function()).
compile_polygon({_,Points}) ->
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

-spec(at_right_of(interval(), float(), float()) -> boolean()).
at_right_of({A, B, C}, X, Y) ->
    A * X + B * Y + C >= 0.

-spec(points_to_intervals(list(point())) -> list(interval())).
points_to_intervals(Points) ->
    [Point1, Point2, {X, Y} | _] = Points,

    % Is poligon specified clockwise
    case at_right_of(interval_for(Point1, Point2), X, Y) of
        true -> points_to_intervals_clockwise(Points);
        false ->
            points_to_intervals_clockwise(lists:reverse(Points))
    end.

-spec(points_to_intervals_clockwise(list(point())) -> list(interval())).
points_to_intervals_clockwise(Points) ->
    Points2 = [lists:last(Points) | Points],
    points_to_intervals_clockwise(Points2, []).

-spec(points_to_intervals_clockwise(list(point()), list(interval())) -> list(interval())).
points_to_intervals_clockwise([Point], Intervals) ->
    Intervals;

points_to_intervals_clockwise([Point1, Point2 | Points], Intervals) ->
    points_to_intervals_clockwise([Point2 | Points], [interval_for(Point1, Point2) | Intervals]).

-spec(interval_for(point(), point()) -> interval()).
interval_for({X1, Y1}, {X2, Y2}) ->
    A = Y1 - Y2,
    B = X2 - X1,
    C = -(A * X1 + B * Y1),
    {A, B, C}.
