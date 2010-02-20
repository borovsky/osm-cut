%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc OSM processor, non-complete objects mode
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
          writer_module,
          write_buffer
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
           writer_module=WriterModule,
           write_buffer = WriterModule:init_client()
          }.


%%--------------------------------------------------------------------
%% @doc Processes OSM message (mode with no "complete objects" flag)
%% @spec process_message(source_element(), #state{}) -> #state{}
%% @end
%%--------------------------------------------------------------------
-spec(process_message(source_element(), #state{}) -> #state{}).
%% start element
process_message({osm, _Attributes, []} = Element, #state{writer_module = Writer,
                                                         write_buffer = WriteBuffer} = State) ->
    Writer:write(WriteBuffer, Element),
    State;

%% end element
process_message(endDocument, #state{writer_module=Writer,
                                    write_buffer = WriteBuffer} = State) ->
    Writer:write(WriteBuffer, endDocument),
    State;

% node element
process_message(#node{id = Id, x = X, y = Y} = Element,
                #state{polygon_function = PolygonFunction,
                       reduced_set = Set,
                       writer_module = Writer,
                       write_buffer = WriteBuffer} = State) ->
    case PolygonFunction(X, Y) of
        true ->
            NewSet = gb_sets:add({node, Id}, Set),
            Writer:write(WriteBuffer, Element),
            State#state{reduced_set = NewSet};
        _ ->
            State
    end;

%%way element
process_message(#way{id = Id, nodes = Nodes} = Way,
                #state{reduced_set = Set,
                       writer_module = Writer,
                       write_buffer = WriteBuffer} = State) ->
    NodesInPolygon = lists:filter(fun(E) -> gb_sets:is_member({node, E}, Set) end, Nodes),
    case NodesInPolygon of
        [] ->
            State;
        List ->
            NewSet = gb_sets:add({way, Id}, Set),
            Writer:write(WriteBuffer, Way#way{nodes = List}),
            State#state{reduced_set = NewSet}
    end;

%% relation element
process_message(#relation{id = Id, members = Members} = Relation,
                #state{reduced_set = Set,
                       writer_module = Writer,
                       write_buffer = WriteBuffer} = State) ->
    MembersInPolygon =
        lists:filter(fun({Type, MemberId, _}) ->
                             gb_sets:is_member({Type, MemberId}, Set) end,
                     Members),
    case MembersInPolygon of
        [] ->
            State;
        List ->
            NewSet = gb_sets:add({relation, Id}, Set),
            Writer:write(WriteBuffer, Relation#relation{members =  List}),
            State#state{reduced_set = NewSet}
    end;

%% other element.
process_message(_Msg, State) ->
    %io:format("Unhandled message: ~p~n", [Msg]),
    State.

