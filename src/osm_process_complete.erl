%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc Nodes processor, with complete objects
%%%
%%% @end
%%% Created : 29 Jan 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_process_complete).

-include("../include/types.hrl").


%% API
-export([init/1, process_message/2]).

-behaviour(osm_processor).

-record(state, {
          polygon_function,
          reduced_set :: gb_set(),
          stored_nodes,
          mode = nodes,
          writer_module,
          ways_to_write,
          written_nodes,
          nodes_to_add = [],
          stored_relations = [],
          relations_to_search = [],
          links_to_parent
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
           stored_nodes = gb_trees:empty(),
           writer_module=WriterModule,
           ways_to_write = [],
           written_nodes = gb_sets:new()}.


%%--------------------------------------------------------------------
%% @doc Processes OSM message (mode with no "complete objects" flag)
%% @spec process_message(source_element(), #state{}) -> #state{}
%% @end
%%--------------------------------------------------------------------
-spec(process_message(source_element(), #state{}) -> #state{}).
%% start element
process_message(#osm{} = Element, #state{writer_module=Writer} = State) ->
    Writer:write(Element),
    State;

% node element
process_message(#node{id  = Id, position = {X, Y}} = Element,
                #state{polygon_function = PolygonFunction,
                       reduced_set = Set,
                       writer_module = Writer,
                       stored_nodes = NodeSet,
                       mode = nodes} = State) ->
    case PolygonFunction(X, Y) of
        true ->
            NewSet = gb_sets:add({node, Id}, Set),
            Writer:write(Element),
            State#state{reduced_set = NewSet};
        _ ->
            NewNodeSet = gb_trees:insert(Id, Element, NodeSet),
            State#state{stored_nodes = NewNodeSet}
    end;

%%way element
process_message(#way{id = Id, nodes = Nodes} = Element,
                #state{reduced_set = Set,
                       ways_to_write = WaysToWrite,
                       nodes_to_add = NodesToAdd,
                       mode = Mode} = State) when (Mode == nodes) orelse
                                                  (Mode == ways) ->
    case nodes_in_poligon(Nodes, Set) of
        out ->
            State#state{mode = ways};
        NodesOut ->
            NewState = write_new_nodes(State, NodesOut),
            NewSet = gb_sets:add({way, Id}, Set),
            NewWaysToWrite = [Element | WaysToWrite],
            NewState#state{reduced_set = NewSet,
                           ways_to_write = NewWaysToWrite,
                           nodes_to_add = Nodes ++ NodesToAdd,
                           mode = ways}
    end;

%% relation element
process_message(#relation{id = Id,
                          members = Members} = Relation,
                #state{mode = relations,
                       reduced_set = Set,
                       relations_to_search = RelationsToSearch,
                       links_to_parent = LinksToParent,
                       stored_relations = StoredRelations} = State) ->
    FilteredMembers =
        lists:filter(fun({Type, MemberId, _}) ->
                             gb_sets:is_member({Type, MemberId}, Set) end,
                     Members),
    case FilteredMembers of
        [] -> % This releation will not have any ways/nodes, so drop them
            NewMembers = lists:filter(fun({Type, _, _}) ->
                                             Type == relation
                                     end, Members),
            NewRelations = [Relation#relation{members = NewMembers} | StoredRelations],
            NewLinks = lists:foldl(fun(E, S) -> add_link_to_parent(E, S, Id) end,
                                   LinksToParent, NewMembers),
            State#state{stored_relations = NewRelations,
                        links_to_parent = NewLinks};
        _ ->
            NewRelations = [Relation | StoredRelations],
            State#state{stored_relations = NewRelations,
                        relations_to_search = [Id | RelationsToSearch]}
    end;

process_message(#relation{} = Msg,
                #state{writer_module = Writer,
                       nodes_to_add = NodesToAdd,
                       reduced_set = ReducedSet,
                       ways_to_write = WaysToWrite} = State) ->
    % Flush collected ways: we processed all of them
    lists:foldl(fun(E, I) ->
                        Writer:write(E),
                        case I rem 1000 of
                            0 -> Writer:synchronize(),
                                 0;
                            _ -> I + 1
                        end
                end, 0, WaysToWrite),
    % Add all collected nodes
    NewSet = lists:foldl(fun(E, S) ->
                                 gb_sets:add({node, E}, S)
                         end, ReducedSet, NodesToAdd),
    
    % Drop collected ways and nodes
    process_message(Msg, State#state{mode = relations,
                                     stored_nodes = [],
                                     ways_to_write = [],
                                     links_to_parent = gb_trees:empty(),
                                     reduced_set = NewSet,
                                     nodes_to_add = []});

%% end element
process_message(endDocument, #state{writer_module=Writer,
                                    reduced_set = ReducedSet,
                                    links_to_parent = LinksToParent,
                                    relations_to_search = RelationsToSearch,
                                    stored_relations = StoredRelations
                                   } = State) ->
    % 1) find all relations to write
    NewSet = calculate_relations(RelationsToSearch, ReducedSet, LinksToParent),

    % 2) Write all selected relations
    lists:foreach(fun(#relation{id = Id} = E) ->
                          case gb_sets:is_member({relation, Id}, NewSet) of
                              true ->
                                  Writer:write(filtered_relation(E, NewSet));
                              _ -> ok
                          end
                  end, StoredRelations),
    
    Writer:write(endDocument),
    State;


%% other element.
process_message(Msg, State) ->
    io:format("Unhandled message: ~p~n", [Msg]),
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================

nodes_in_poligon(Nodes, Set) ->
    nodes_in_poligon(Nodes, Set, [], []).

nodes_in_poligon([E | Tail], Set, NodesIn, NodesOut) ->
    case gb_sets:is_member({node, E}, Set) of
        true ->
            nodes_in_poligon(Tail, Set, [E | NodesIn], NodesOut);
        false ->
            nodes_in_poligon(Tail, Set, NodesIn, [E | NodesOut])
    end;
nodes_in_poligon([], _, [], NodesOut) ->
    out;

nodes_in_poligon([], _, _, NodesOut) ->
    NodesOut.
            
write_new_nodes(State, []) ->
    State;

write_new_nodes(#state{writer_module = WriterModule,
                       written_nodes = WrittenNodes,
                       stored_nodes = StoredNodes} = State, [Head | Tail]) ->
    case gb_sets:is_member(Head, WrittenNodes) of
        true -> write_new_nodes(State, Tail);
        _ ->
            Node = gb_trees:get(Head, StoredNodes),
            WriterModule:write(Node),
            NewState = State#state{written_nodes  = gb_sets:add(Head, WrittenNodes)},
            write_new_nodes(NewState, Tail)
    end.

add_link_to_parent({relation, Id, _}, S, ParentId) ->
    case gb_trees:lookup(Id, S) of
        none ->
            gb_trees:insert(Id, [ParentId], S);
        {value, Val} ->
            gb_trees:update(Id, [ParentId | Val], S)
    end.

calculate_relations([H | T], ReducedSet, LinksToParent) ->
    case gb_sets:is_member({relation, H}, ReducedSet) of
        false ->
            NewSet = gb_sets:insert({relation, H}, ReducedSet),
            case gb_trees:lookup(H, LinksToParent) of
                none ->
                    calculate_relations(T, NewSet, LinksToParent);
                {value, L} ->
                    calculate_relations(L ++ T, NewSet, LinksToParent)
            end;
        _ -> calculate_relations(T, ReducedSet, LinksToParent)
    end;

calculate_relations([], ReducedSet, _) ->
    ReducedSet.

filtered_relation(#relation{} = Relation, Set) ->
    Relation.
