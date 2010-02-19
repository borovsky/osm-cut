%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(osm_polygon_compiler).

-export([compile_polygon/2, compile/1]).
-include("types.hrl").
-include_lib("kernel/include/file.hrl").


-record(bounding_box, {
          xmin = 0.0 :: float(), 
          ymin = 0.0 :: float(),
          xmax = 0.0 :: float(), 
          ymax = 0.0 :: float()
         }).
-record(interval, {
          bounding_box = #bounding_box{} :: #bounding_box{},
          a = 0.0 :: float(), %A * X + B * Y + C = 0  for all points in interval
          b = 0.0 :: float(),
          c = 0.0 :: float()}).
-type(intervals() :: list(#interval{})).

-type(interval_1d() :: {float(), float()}).
-type(intervals_1d() :: [interval_1d()]).


-type(side() :: left | right | top | bottom).

-record(geotree_compile_data, {
          bbox = #bounding_box{} :: #bounding_box{},
          in = [] :: intervals(),
          at_left = [] :: intervals_1d(),
          full_at_left = 0 :: non_neg_integer(),
          at_right = [] :: intervals_1d(),
          full_at_right = 0 :: non_neg_integer(),
          at_top = [] :: intervals_1d(),
          full_at_top = 0 :: non_neg_integer(),
          at_bottom = [] :: intervals_1d(),
          full_at_bottom = 0 :: non_neg_integer(),
          level = 0
         }).

-record(geotree_leaf, {
          side = left :: side(),
          counted = 0 :: non_neg_integer(),
          in_intervals :: intervals(),
          side_intervals :: intervals_1d() % List of intervals in projection to X/Y axis
         }).

-record(geotree_branch, {
          left_top :: #geotree_leaf{} | {geotree_branch | geotree_leaf, any(), any(), any(), any()} | in, %tree_part() 
          left_bottom :: #geotree_leaf{} | {geotree_branch | geotree_leaf, any(), any(), any(), any()}| in, %tree_part() 
          right_top :: #geotree_leaf{} | {geotree_branch | geotree_leaf, any(), any(), any(), any()}| in, %tree_part() 
          right_bottom :: #geotree_leaf{} | {geotree_branch | geotree_leaf, any(), any(), any(), any()}| in %tree_part() 
         }).


-type(geotree_part() :: #geotree_leaf{} | #geotree_branch{} | in | out).

-record(geotree_root, {
          bbox :: #bounding_box{},
          inner_tree :: geotree_part()
         }).

-define(MAX_TREE_LEVEL, 7).
-define(MAX_INT_INTERVALS_PER_LEAF, 20).

%%--------------------------------------------------------------------
%% @doc Reads polygon file and creates polygon function
%% @spec compile_polygon(string(), property_list()) -> polygon_function()
%% @end
%%--------------------------------------------------------------------
-spec(compile_polygon(string(), property_list()) -> polygon_function()).
compile_polygon(SourceFile, _Options) ->
    io:format("~p: Compiling polygon~n", [erlang:localtime()]),
    {ok, File} = file:open(SourceFile, [read]),
    io:get_line(File, ""), %Ignore first line
    Polygons = read_polygons(File, []),
    file:close(File),
    compile(Polygons).


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
    Tree = geotree_for_points(Points),

    fun(X, Y) -> is_point_in(Tree, X, Y) end.

%%--------------------------------------------------------------------
%% @doc Checks if polygon in
%% @spec is_point_in(#geotree_root{}, float(), float()) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(is_point_in(#geotree_root{}, float(), float()) -> boolean()).
is_point_in(#geotree_root{bbox = BBox,
                          inner_tree = InnerTree}, X, Y) ->
    case is_inside_bbox(BBox, X, Y) of
       true -> is_point_in(InnerTree, BBox, X, Y);
       _ -> false
    end.

-spec(is_point_in(geotree_part(), #bounding_box{}, float(), float()) ->
             boolean()).
is_point_in(in, _, _, _) ->
    %io:format("in~n"), 
    true;

is_point_in(out, _, _, _) ->
    %io:format("out~n"), 
    false;

% going down by tree
is_point_in(#geotree_branch{left_top = LT,
                            right_top = RT,
                            left_bottom = LB,
                            right_bottom = RB},
            #bounding_box{xmin = Xmin, ymin = Ymin,
                          xmax = Xmax, ymax = Ymax}, X, Y) ->
    Xs = (Xmin + Xmax) / 2,
    Ys = (Ymin + Ymax) / 2,
    case X < Xs of
        true ->
            case Y < Ys of
                true ->
                    is_point_in(LB, #bounding_box{xmin = Xmin, ymin = Ymin, xmax = Xs, ymax = Ys}, X, Y);
                false ->
                    is_point_in(LT, #bounding_box{xmin = Xmin, ymin = Ys, xmax = Xs, ymax = Ymax}, X, Y)
            end;
        false ->
            case Y < Ys of
                true ->
                    is_point_in(RB, #bounding_box{xmin = Xs, ymin = Ymin, xmax = Xmax, ymax = Ys}, X, Y);
                false ->
                    is_point_in(RT, #bounding_box{xmin = Xs, ymin = Ys, xmax = Xmax, ymax = Ymax}, X, Y)
            end
    end;
is_point_in(#geotree_leaf{side = Side,
                          counted = Counted,
                          in_intervals = InIntervals,
                          side_intervals = SideIntervals},
            #bounding_box{}, X, Y) when Side == left orelse Side == right ->
    case count_inside_intersects(Side, InIntervals, X, Y) of
        on -> true;
        Cin ->
            % Calculate count of outside intervals at left/right on node
            Cout = lists:foldl(fun({Ymin, Ymax}, S) ->
                                       case Ymin =< Y andalso Y < Ymax of
                                           true -> S + 1;
                                           _ -> S
                                       end
                               end, 0, SideIntervals),
            (Cin + Cout + Counted) rem 2 == 1
    end;

is_point_in(#geotree_leaf{side = Side,
                          counted = Counted,
                          in_intervals = InIntervals,
                          side_intervals = SideIntervals},
            #bounding_box{}, X, Y) -> %  when Side == top orelse Side == bottom
    case count_inside_intersects(Side, InIntervals, X, Y) of
        on -> true;
        Cin ->
            % Calculate count of outside intervals at left/right on node
            Cout = lists:foldl(fun({Xmin, Xmax}, S) ->
                                       case Xmin =< X andalso X < Xmax of
                                           true -> S + 1;
                                           _ -> S
                                       end
                               end, 0, SideIntervals),
            (Cin + Cout + Counted) rem 2 == 1
    end;
is_point_in(D, _, _, _) ->
    io:format("Error: ~p", [D]),
    false.

-spec(geotree_for_points(points()) -> #geotree_root{}).
geotree_for_points(Points) ->
    Intervals = points_to_intervals(Points),
    BBox = bounding_box_for(Points),
    InnerTree = geotree_for(#geotree_compile_data{in = Intervals, bbox = BBox}),
    #geotree_root{bbox = BBox, inner_tree = InnerTree}.


% If we have no intervals inside, so current box filly inside or outside
-spec(geotree_for(#geotree_compile_data{}) -> geotree_part()).
% We reached max tree level. Create leaf with minimal check efforts
geotree_for(#geotree_compile_data{in = [],
                                  bbox = #bounding_box{xmin = Xmin, ymin = Ymin}} = Data) ->
    case is_inside(Data, Xmin, Ymin) of
        true ->
            io:format(string:copies("i", 1 bsl (?MAX_TREE_LEVEL - Data#geotree_compile_data.level))),
            in;
        false ->
            io:format(string:copies("o", 1 bsl (?MAX_TREE_LEVEL - Data#geotree_compile_data.level))),
            out
    end;

geotree_for(#geotree_compile_data{level = ?MAX_TREE_LEVEL} = Data) ->
    io:format("?"),
    geotree_leaf_for(Data);

geotree_for(#geotree_compile_data{bbox = #bounding_box{
                                    xmin = Xmin,
                                    ymin = Ymin,
                                    xmax = Xmax,
                                    ymax = Ymax},
                                  in = In} = Data) ->
    case length(In) =< ?MAX_INT_INTERVALS_PER_LEAF of
        true ->
            io:format(string:copies("?", 1 bsl (?MAX_TREE_LEVEL - Data#geotree_compile_data.level))),
            geotree_leaf_for(Data);
        false ->
            Xs = (Xmin + Xmax) / 2,
            Ys = (Ymin + Ymax) / 2,
            #geotree_branch{left_top = geotree_for(reduce_for_subtree(Data, Xmin, Ys, Xs, Ymax)),
                            right_top = geotree_for(reduce_for_subtree(Data, Xs, Ys, Xmax, Ymax)),
                            left_bottom = geotree_for(reduce_for_subtree(Data, Xmin, Ymin, Xs, Ys)),
                            right_bottom = geotree_for(reduce_for_subtree(Data, Xs, Ymin, Xmax, Ys))}
        end.

geotree_leaf_for(#geotree_compile_data{in = In} = Data) ->
    {Side, Intervals, CountFull} = select_minimal_side(Data),
    InUpd = in_intervals_for_ray(Side, In),
    #geotree_leaf{side = Side, counted = CountFull, in_intervals = InUpd, side_intervals = Intervals}.

is_inside(#geotree_compile_data{} = Data, X, Y) ->
    {Side, Intervals, CountFull} = select_minimal_side(Data),
    (count_at_side(Side, Intervals, X, Y) + CountFull) rem 2 == 1.

is_inside_bbox(#bounding_box{
                            xmin = Xmin,
                            ymin = Ymin,
                            xmax = Xmax,
                            ymax = Ymax
                           }, X, Y) ->
    X >= Xmin andalso X =< Xmax andalso Y >= Ymin andalso Y =< Ymax.

-spec(select_minimal_side(#geotree_compile_data{}) ->
             {side(), intervals_1d(), non_neg_integer()}).
select_minimal_side(#geotree_compile_data{at_left = AtLeft, full_at_left = FullAtLeft,
                                          at_right = AtRight, full_at_right = FullAtRight,
                                          at_top = AtTop, full_at_top = FullAtTop,
                                          at_bottom = AtBottom, full_at_bottom = FullAtBottom
                                         }) ->
    R1 = select_minimal_side({left, AtLeft, FullAtLeft},
                             {right, AtRight, FullAtRight}),
    R2 = select_minimal_side({top, AtTop, FullAtTop}, R1),
    select_minimal_side({bottom, AtBottom, FullAtBottom}, R2).

select_minimal_side({_, Intervals1, _} = S1,
                    {_, Intervals2, _} = S2) ->
    if length(Intervals1) < length(Intervals2) ->
            S1;
       true -> S2
    end.

-spec(reduce_for_subtree(#geotree_compile_data{}, float(), float(), float(), float()) -> #geotree_compile_data{}).
reduce_for_subtree(#geotree_compile_data{level = Level,
                                         at_left = AtLeft, full_at_left = FullAtLeft,
                                         at_right = AtRight, full_at_right = FullAtRight,
                                         at_top = AtTop, full_at_top = FullAtTop,
                                         at_bottom = AtBottom, full_at_bottom = FullAtBottom,
                                         in = In},
                   Xmin, Ymin, Xmax, Ymax) ->
    {NewAtLeft, NewFullAtLeft} = reduce_side(Ymin, Ymax, AtLeft, FullAtLeft),
    {NewAtRight, NewFullAtRight} = reduce_side(Ymin, Ymax, AtRight, FullAtRight),
    {NewAtTop, NewFullAtTop} = reduce_side(Xmin, Xmax, AtTop, FullAtTop),
    {NewAtBottom, NewFullAtBottom} = reduce_side(Xmin, Xmax, AtBottom, FullAtBottom),
    Base = #geotree_compile_data{level = Level + 1,
                                 bbox = #bounding_box{xmin = Xmin, ymin = Ymin, xmax = Xmax, ymax = Ymax},
                                 at_left = NewAtLeft, full_at_left = NewFullAtLeft,
                                 at_right = NewAtRight, full_at_right = NewFullAtRight,
                                 at_top = NewAtTop, full_at_top = NewFullAtTop,
                                 at_bottom = NewAtBottom, full_at_bottom = NewFullAtBottom,
                                 in = []
                                },
    reduce_internal_intervals(In, Base).

reduce_side(BlockMin, BlockMax, List, FullCount) ->
    lists:foldl(fun({Min, Max} = E, {L, C} = S) ->
                        case Max < BlockMin orelse BlockMax =< Min of % Out of side
                            true ->
                                S;
                            _ ->
                                case Min =< BlockMin andalso BlockMax < Max of
                                    true ->
                                        {L, C + 1};
                              _  -> {[E | L], C}
                                end
                        end
                end, {[], FullCount}, List),
    {List, FullCount}.

reduce_internal_intervals([#interval{bounding_box = #bounding_box{xmin = Xmin, ymin = Ymin,
                                                                  xmax = Xmax, ymax = Ymax}} = H | T],
                          #geotree_compile_data{bbox = BBox} = D) ->
    case classify_interval(BBox, H) of
        in ->
            reduce_internal_intervals(T, D#geotree_compile_data{in = [H | D#geotree_compile_data.in]});
        out ->
            reduce_internal_intervals(T, D);
        List ->
            R = lists:foldl(
              fun(Type, S) ->
                      case Type of
                          left ->
                              S#geotree_compile_data{
                                at_left = [{Ymin, Ymax} | D#geotree_compile_data.at_left]};
                          full_left ->
                              S#geotree_compile_data{
                                full_at_left = D#geotree_compile_data.full_at_left + 1};
                          right ->
                              S#geotree_compile_data{
                                at_right = [{Ymin, Ymax} | D#geotree_compile_data.at_right]};
                          full_right ->
                              S#geotree_compile_data{
                                full_at_right = D#geotree_compile_data.full_at_right + 1};
                          top ->
                              S#geotree_compile_data{
                                at_top = [{Xmin, Xmax} | D#geotree_compile_data.at_top]};
                          full_top ->
                              S#geotree_compile_data{
                                full_at_top = D#geotree_compile_data.full_at_top + 1};
                          bottom ->
                              S#geotree_compile_data{
                                at_bottom = [{Xmin, Xmax} | D#geotree_compile_data.at_bottom]};
                          full_bottom ->
                              S#geotree_compile_data{
                                full_at_bottom = D#geotree_compile_data.full_at_bottom + 1}
                      end
              end, D, List),
            reduce_internal_intervals(T, R)
    end;

reduce_internal_intervals([], D) ->
    D.

-spec(classify_interval(#bounding_box{}, #interval{}) -> list(atom()) | in | out).
classify_interval(BBox, I) ->
    case could_be_intersected_by_ray(BBox, I#interval.bounding_box) of
        false -> out; % interval can't be intersected by horizontal or vertical ray
        true ->
            case is_intersects(BBox, I) of
                true -> in;
                _  -> L1 = case is_at_top(BBox, I) of
                               true -> [top];
                               full -> [full_top];
                               false -> []
                           end,
                      L2 = case is_at_bottom(BBox, I) of
                               true -> [bottom];
                               full -> [full_bottom];
                               false -> []
                           end,
                      L3 = case is_at_left(BBox, I) of
                               true -> [left];
                               full -> [full_left];
                               false -> []
                           end,
                      L4 = case is_at_right(BBox, I) of
                               true -> [right];
                               full -> [full_right];
                               false -> []
                           end,
                      case L1 ++ L2 ++ L3 ++ L4 of
                          [] -> in;
                          R  -> R
                      end
            end
    end.

is_intersects(#bounding_box{xmin = Xmin,
                            ymin = Ymin,
                            xmax = Xmax,
                            ymax = Ymax
                           },
              #interval{bounding_box= #bounding_box{xmin = IXmin,
                                                    ymin = IYmin,
                                                    xmax = IXmax}}) ->
    % Simple, not accurate,... but work
    Xmin >= IXmax andalso IXmin > Xmax andalso (Ymin >= IYmin andalso IYmin > Ymax).

is_at_top(#bounding_box{xmin = Xmin,
                        xmax = Xmax,
                        ymax = Ymax
                       },
          #interval{bounding_box= #bounding_box{xmin = IXmin,
                                                ymin = IYmin,
                                                xmax = IXmax}}) ->
    if
        IYmin > Ymax  ->
            if
                IXmin =< Xmin andalso Xmax < IXmax ->
                    full;
                Xmin < IXmax andalso Xmax >= IXmin ->
                    true;
                true -> false
            end;
        true -> false
    end.

is_at_bottom(#bounding_box{xmin = Xmin,
                           ymin = Ymin,
                           xmax = Xmax,
                           ymax = _Ymax
                          },
             #interval{bounding_box= #bounding_box{xmin = IXmin,
                                                   xmax = IXmax,
                                                   ymax = IYmax}}) ->
    if
        IYmax < Ymin  ->
            if
                IXmin =< Xmin andalso Xmax < IXmax ->
                    full;
                Xmin < IXmax andalso Xmax >= IXmin ->
                    true;
                true -> false
            end;
        true -> false
    end.

is_at_left(#bounding_box{xmin = Xmin,
                         ymin = Ymin,
                         xmax = _Xmax,
                         ymax = Ymax
                        },
           #interval{bounding_box= #bounding_box{ymin = IYmin,
                                                 xmax = IXmax,
                                                 ymax = IYmax}}) ->
    if
        IXmax < Xmin  ->
            if
                IYmin =< Ymin andalso Ymax < IYmax ->
                    full;
                Ymin < IYmax andalso Ymax >= IYmin ->
                    true;
                true -> false
            end;
        true -> false
    end.

is_at_right(#bounding_box{xmin = _Xmin,
                          ymin = Ymin,
                          xmax = Xmax,
                          ymax = Ymax
                         },
            #interval{bounding_box= #bounding_box{xmin = IXmin,
                                                  ymin = IYmin,
                                                  ymax = IYmax}}) ->
    if
        IXmin > Xmax  ->
            if
                IYmin =< Ymin andalso Ymax < IYmax ->
                    full;
                Ymin < IYmax andalso Ymax >= IYmin ->
                    true;
                true -> false
            end;
        true -> false
    end.

could_be_intersected_by_ray(#bounding_box{xmin = Xmin,
                                          ymin = Ymin,
                                          xmax = Xmax,
                                          ymax = Ymax
                                         },
                           #bounding_box{xmin = IXmin,
                                         ymin = IYmin,
                                         xmax = IXmax,
                                         ymax = IYmax}) ->
    
    (IXmin < Xmax andalso IXmax >= Xmin) orelse
        (IYmin < Ymax andalso IYmax >= Ymin).

%%--------------------------------------------------------------------
%% @doc Calculates count on intervals inside submap at (Side) of (X,Y)
%% (or atom 'in' if point on interval)
%% @spec count_inside_intersects(side(), intervals(), float(), float()) -> non_neg_integer() | on
%% @end
%% --------------------------------------------------------------------
-spec(count_inside_intersects(side(), intervals(), float(), float()) -> non_neg_integer() | on).
count_inside_intersects(Side, InIntervals, X, Y) ->
    count_inside_intersects(Side, X, Y, InIntervals, 0).

count_inside_intersects(left, X, Y, [#interval{bounding_box = #bounding_box{ymin = Ymin, ymax = Ymax, xmin = Xmin},
                                                           a = A, b = B, c = C} | T], Count) ->
    case (Ymin =< Y) andalso (Y =< Ymax) andalso (Xmin =< X) of
        true ->
            R = A * X + B * Y + C,
            if
                R == 0 -> on;
                (R > 0) andalso (Y < Ymax) ->
                    count_inside_intersects(left, X, Y, T, Count + 1);
                true ->
                    count_inside_intersects(left, X, Y, T, Count)
            end;
        _ -> count_inside_intersects(left, X, Y, T, Count)
    end;

count_inside_intersects(right, X, Y, [#interval{bounding_box = #bounding_box{ymin = Ymin, ymax = Ymax, xmax = Xmax},
                                                           a = A, b = B, c = C} | T], Count) ->
    case (Ymin =< Y) andalso (Y =< Ymax) andalso (Xmax >= X) of
        true ->
            R = A * X + B * Y + C,
            if
                R == 0 -> on;
                (R > 0) andalso (Y < Ymax) ->
                    count_inside_intersects(right, X, Y, T, Count + 1);
                true ->
                    count_inside_intersects(right, X, Y, T, Count)
            end;
        _ -> count_inside_intersects(right, X, Y, T, Count)
    end;

count_inside_intersects(top, X, Y, [#interval{bounding_box = #bounding_box{xmin = Xmin, xmax = Xmax, ymax = Ymax},
                                                           a = A, b = B, c = C} | T], Count) ->
    case (Xmin =< X) andalso (X =< Xmax) andalso (Y =< Ymax) of
        true ->
            R = A * X + B * Y + C,
            if
                R == 0 -> on;
                (R > 0) andalso (X < Xmax) ->
                    count_inside_intersects(top, X, Y, T, Count + 1);
                true ->
                    count_inside_intersects(top, X, Y, T, Count)
            end;
        _ -> count_inside_intersects(top, X, Y, T, Count)
    end;

count_inside_intersects(bottom, X, Y, [#interval{bounding_box = #bounding_box{xmin = Xmin, xmax = Xmax, ymin = Ymin},
                                                           a = A, b = B, c = C} | T], Count) ->
    case (Xmin =< X) andalso (X =< Xmax) andalso (Y >= Ymin) of
        true ->
            R = A * X + B * Y + C,
            if
                R == 0 -> on;
                (R > 0) andalso (X < Xmax) ->
                    count_inside_intersects(bottom, X, Y, T, Count + 1);
                true ->
                    count_inside_intersects(bottom, X, Y, T, Count)
            end;
        _ -> count_inside_intersects(bottom, X, Y, T, Count)
    end;

count_inside_intersects(_, _, _, [], Count) ->
    Count.

%%--------------------------------------------------------------------
%% @doc Calculates count of intervals intersected by ray to side from point (X, Y)
%% @spec count_at_side(side(), intervals_1d(), float(), float()) -> non_neg_integer()
%% @end
%%--------------------------------------------------------------------
-spec(count_at_side(side(), intervals_1d(), float(), float()) ->
             non_neg_integer()).
count_at_side(left, Intervals, _X, Y) ->
    count_1d_side(Intervals, Y, 0);
count_at_side(right, Intervals, _X, Y) ->
    count_1d_side(Intervals, Y, 0);
count_at_side(top, Intervals, X, _Y) ->
    count_1d_side(Intervals, X, 0);
count_at_side(bottom, Intervals, X, _Y) ->
    count_1d_side(Intervals, X, 0).

-spec(count_1d_side(intervals_1d(), float(), non_neg_integer()) ->
             non_neg_integer()).
count_1d_side([], _, Count) ->
    Count;

count_1d_side([{Min, Max} | T], X, Count) ->
    case Min =< X andalso X < Max of
        true -> count_1d_side(T, X, Count + 1);
        _ -> count_1d_side(T, X, Count)
    end.

%%--------------------------------------------------------------------
%% @doc Translates interval equation: if ray from point intersects interval, AX+BY+C > 0
%% @spec in_intervals_for_ray(side(), intervals()) -> intervals()
%% @end
%%--------------------------------------------------------------------
-spec(in_intervals_for_ray(side(), intervals()) -> intervals()).
in_intervals_for_ray(left, Intervals) ->
    lists:map(fun(#interval{a = A, b = B, c = C} = I) ->
                      case A < 0 of
                          true ->
                              I#interval{a = -A, b = -B, c = -C};
                          _ -> I
                      end
              end, Intervals);
in_intervals_for_ray(right, Intervals) ->
    lists:map(fun(#interval{a = A, b = B, c = C} = I) ->
                      case A > 0 of
                          true ->
                              I#interval{a = -A, b = -B, c = -C};
                          _ -> I
                      end
              end, Intervals);
in_intervals_for_ray(top, Intervals) ->
    lists:map(fun(#interval{a = A, b = B, c = C} = I) ->
                      case B > 0 of
                          true ->
                              I#interval{a = -A, b = -B, c = -C};
                          _ -> I
                      end
              end, Intervals);
in_intervals_for_ray(bottom, Intervals) ->
    lists:map(fun(#interval{a = A, b = B, c = C} = I) ->
                      case B < 0 of
                          true ->
                              I#interval{a = -A, b = -B, c = -C};
                          _ -> I
                      end
              end, Intervals).

%%--------------------------------------------------------------------
%% @doc Converts ordered set of points to list of intervals
%% @spec points_to_intervals(list(point())) -> list(interval())
%% @end
%%--------------------------------------------------------------------
-spec(points_to_intervals(list(point())) -> list(#interval{})).
points_to_intervals(Points) ->
    points_to_intervals(Points, []).

%%--------------------------------------------------------------------
%% @doc Converts ordered set of points to list of intervals
%% @spec points_to_intervals(list(point()), list(interval())) -> list(interval())
%% @end
%%--------------------------------------------------------------------
-spec(points_to_intervals(list(point()), list(#interval{})) -> list(#interval{})).
points_to_intervals([_Point], Intervals) ->
    Intervals;

points_to_intervals([Point1, Point2 | Points], Intervals) ->
    points_to_intervals([Point2 | Points], [interval_for(Point1, Point2) | Intervals]).

%%--------------------------------------------------------------------
%% @doc Creates interval from start and end line points
%% @spec interval_for(point(), point()) -> interval()
%% @end
%%--------------------------------------------------------------------
-spec(interval_for(point(), point()) -> #interval{}).
interval_for({X1, Y1}, {X2, Y2}) ->
    A = Y2 - Y1,
    B = X1 - X2,
    C = -(A * X1 + B * Y1),
    BBox = bounding_box_for([{X1, Y1}, {X2, Y2}]),
    #interval{a = A, b = B, c = C, bounding_box = BBox}.

%%--------------------------------------------------------------------
%% @doc Creates bounding box for list of points
%% @spec bounding_box_for(list(point())) -> #bounding_box{}
%% @end
%%--------------------------------------------------------------------
-spec(bounding_box_for(list(point())) -> #bounding_box{}).
bounding_box_for([{X, Y} | T]) ->
    bounding_box_for(#bounding_box{xmin = X, xmax = X, ymin = Y, ymax = Y}, T).

-spec(bounding_box_for(#bounding_box{}, list(point())) -> #bounding_box{}).
bounding_box_for(#bounding_box{xmin = Xmin, xmax = Xmax, ymin = Ymin, ymax = Ymax}, [{X, Y} | T]) ->
    {NXmin, NXmax} = bounding_box_1d(Xmin, Xmax, X),
    {NYmin, NYmax} = bounding_box_1d(Ymin, Ymax, Y),
    bounding_box_for(#bounding_box{xmin = NXmin, xmax = NXmax, ymin = NYmin, ymax = NYmax}, T);
bounding_box_for(BBox, []) ->
    BBox.

%%--------------------------------------------------------------------
%% @doc Creates bounding box on line
%% @spec bounding_box_1d(float(), float(), float()) -> {float(), float()}
%% @end
%%--------------------------------------------------------------------
-spec(bounding_box_1d(float(), float(), float()) -> {float(), float()}).
bounding_box_1d(Xmin, Xmax, X) ->
    case X < Xmin of
        true -> {X, Xmax};
        _ ->
            case X > Xmax of
                true -> {Xmin, X};
                _ -> {Xmin, Xmax}
            end
    end.


