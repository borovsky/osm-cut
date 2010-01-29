%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_process_non_complete).

-include("types.hrl").


%% API
-export([init/1, process_message/2]).

-behaviour(osm_processor).

-record(state, {
          polygon_function,
          reduced_set,
          writer_module
         }).

%%%===================================================================
%%% API
%%%===================================================================

-spec(init(property_list()) -> #state{}).
init(Options) ->
    PolygonFunction = proplists:get_value(polygon, Options),
    WriterModule = proplists:get_value(writer_module, Options, osm_writer),
    
    #state{polygon_function = PolygonFunction,
                reduced_set = gb_sets:new(),
                writer_module=WriterModule} .


%%--------------------------------------------------------------------
%% @doc Processes OSM message (mode with no "complete objects" flag)
%% @spec process_message(source_element(), #state{}) -> #state{}
%% @end
%%--------------------------------------------------------------------
-spec(process_message(source_element(), #state{}) -> #state{}).
%% start element
process_message({osm, _Attributes, []} = Element, #state{writer_module=Writer} = State) ->
    Writer:write(Element),
    State;

%% end element
process_message(endDocument, #state{writer_module=Writer} = State) ->
    Writer:write(endDocument),
    State;

% node element
process_message({node, Id, {X, Y}, _, _} = Element, #state{polygon_function = PolygonFunction,
         reduced_set = Set, writer_module = Writer} = State) ->
    case PolygonFunction(X, Y) of
        true ->
            NewSet = gb_sets:add({node, Id}, Set),
            Writer:write(Element),
            State#state{reduced_set = NewSet};
        _ ->
            State
    end;

%%way element
process_message({way, Id, Nodes, Attributes, Tags}, #state{reduced_set = Set, writer_module = Writer} = State) ->
    NodesInPolygon = lists:filter(fun(E) -> gb_sets:is_member({node, E}, Set) end, Nodes),
    case NodesInPolygon of
        [] ->
            State;
        List ->
            NewSet = gb_sets:add({way, Id}, Set),
            Writer:write({way, Id, List, Attributes, Tags}),
            State#state{reduced_set = NewSet}
    end;

%% relation element
process_message({relation, Id, Members, Attributes, Tags}, #state{reduced_set = Set, writer_module = Writer} = State) ->
    MembersInPolygon =
        lists:filter(fun({Type, MemberId, _}) ->
                             gb_sets:is_member({Type, MemberId}, Set) end,
                     Members),
    case MembersInPolygon of
        [] ->
            State;
        List ->
            NewSet = gb_sets:add({relation, Id}, Set),
            Writer:write({relation, Id, List, Attributes, Tags}),
            State#state{reduced_set = NewSet}
    end;

%% other element.
process_message(_Msg, State) ->
    %io:format("Unhandled message: ~p~n", [Msg]),
    State.

