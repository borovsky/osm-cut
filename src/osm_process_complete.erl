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
          polygon_function :: polygon_function(), % Function for check if node inside
          reduced_set = osm_set:empty() :: osm_set(), % List of written items
          stored_nodes, % Nodes not written, but stored for way processing
          mode = nodes :: nodes | ways | relations, % current mode: nodes, ways or relations
          writer_module :: atom(), % Module for write result
          ways_to_write :: list(source_element()), % Ways that should be written when we spot first relation
          nodes_to_write = gb_sets:new() :: gb_set(), % Set of nodes for writ (but outside poligon)
          stored_relations, % List of relations for future processing
          relations_to_search = [], % List of relations that included because they have selected nodes / ways
          % Link to list of relations, that should be included if key relation included
          links_to_parent = gb_trees:empty() :: gb_tree() 
         }).

%%%===================================================================
%%% API
%%%===================================================================

-spec(init(property_list()) -> #state{}).
init(Options) ->
    PolygonFunction = proplists:get_value(polygon, Options),
    WriterModule = proplists:get_value(writer_module, Options, osm_writer),
    
    #state{polygon_function = PolygonFunction,
           reduced_set = osm_set:empty(),
           stored_nodes = osm_node_storage:create("nodes"),
           writer_module=WriterModule,
           ways_to_write = []
          }.


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
            NewSet = osm_set:add({node, Id}, Set),
            Writer:write(Element),
            State#state{reduced_set = NewSet};
        _ ->
            NewNodeSet = osm_node_storage:add(Element, NodeSet),
            State#state{stored_nodes = NewNodeSet}
    end;

%%way element
process_message(#way{id = Id, nodes = Nodes} = Element,
                #state{reduced_set = Set,
                       ways_to_write = WaysToWrite,
                       mode = ways} = State) ->
    case nodes_in_poligon(Nodes, Set) of
        out ->
            State#state{mode = ways};
        NodesOut ->
            NewState = calculate_nodes_to_write(State, NodesOut),
            NewSet = osm_set:add({way, Id}, Set),
            NewWaysToWrite = [Element | WaysToWrite],
            NewState#state{reduced_set = NewSet,
                           ways_to_write = NewWaysToWrite,
                           mode = ways}
    end;

process_message(#way{} = Element,
                #state{mode = nodes} = State) ->
    io:format("~p: nodes processed~n", [erlang:localtime()]),
    io:format("  Set size: ~p~n", [erts_debug:size(State#state.reduced_set)]),
    io:format("  Nodes size: ~p~n", [erts_debug:flat_size(State#state.stored_nodes)]),
    io:format("  State size: ~p~n", [erts_debug:flat_size(State)]),
    process_message(Element, State#state{mode=ways});

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
                             osm_set:is_member({Type, MemberId}, Set) end,
                     Members),
    case FilteredMembers of
        [] -> % This releation will not have any ways/nodes, so drop them
            NewMembers = lists:filter(fun({Type, _, _}) ->
                                             Type == relation
                                     end, Members),
            NewRelations = osm_node_storage:add(Relation#relation{members = NewMembers}, StoredRelations),
            NewLinks = lists:foldl(fun(E, S) -> add_link_to_parent(E, S, Id) end,
                                   LinksToParent, NewMembers),
            State#state{stored_relations = NewRelations,
                        links_to_parent = NewLinks};
        _ ->
            NewRelations = osm_node_storage:add(Relation, StoredRelations),
            State#state{stored_relations = NewRelations,
                        relations_to_search = [Id | RelationsToSearch]}
    end;

process_message(#relation{} = Msg,
                #state{writer_module = Writer,
                       reduced_set = ReducedSet,
                       ways_to_write = WaysToWrite,
                       nodes_to_write = NodesToWrite,
                       stored_nodes = StoredNodes} = State) ->
    % Flush all collected nodes
    NewSet = osm_node_storage:fold(
               fun(#node{id = Id} = E, S) ->
                       case gb_sets:is_member(Id, NodesToWrite) of
                           true ->
                               Writer:write(E),
                               osm_set:add({node, Id}, S);
                           _ -> S
                       end end, ReducedSet, StoredNodes),
    
    % Flush collected ways: we processed all of them
    lists:foldl(fun(E, I) ->
                        Writer:write(E),
                        case I rem 1000 of
                            0 -> Writer:synchronize(),
                                 0;
                            _ -> I + 1
                        end
                end, 0, WaysToWrite),
    
    % Drop collected ways and nodes
    process_message(Msg, State#state{mode = relations,
                                     stored_nodes = osm_node_storage:close(StoredNodes),
                                     ways_to_write = [],
                                     links_to_parent = gb_trees:empty(),
                                     stored_relations = osm_node_storage:create("relations"),
                                     reduced_set = NewSet});

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
    osm_node_storage:fold(fun(#relation{id = Id} = E, _) ->
                                  case osm_set:is_member({relation, Id}, NewSet) of
                                      true ->
                                          Writer:write(filtered_relation(E, NewSet));
                                      _ -> ok
                                  end
                          end, any, StoredRelations),
    
    Writer:write(endDocument),
    State#state{stored_relations = osm_node_storage:close(StoredRelations)};


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
    case osm_set:is_member({node, E}, Set) of
        true ->
            nodes_in_poligon(Tail, Set, [E | NodesIn], NodesOut);
        false ->
            nodes_in_poligon(Tail, Set, NodesIn, [E | NodesOut])
    end;
nodes_in_poligon([], _, [], _) ->
    out;

nodes_in_poligon([], _, _, NodesOut) ->
    NodesOut.
            
calculate_nodes_to_write(State, []) ->
    State;

calculate_nodes_to_write(#state{nodes_to_write = NodesToWrite} = State, [Head | Tail]) ->
    case gb_sets:is_member(Head, NodesToWrite) of
        true -> calculate_nodes_to_write(State, Tail);
        _ ->
            NewState = State#state{nodes_to_write = gb_sets:add(Head, NodesToWrite)},
            calculate_nodes_to_write(NewState, Tail)
    end.

add_link_to_parent({relation, Id, _}, S, ParentId) ->
    case gb_trees:lookup(Id, S) of
        none ->
            gb_trees:insert(Id, [ParentId], S);
        {value, Val} ->
            gb_trees:update(Id, [ParentId | Val], S)
    end.

calculate_relations([H | T], ReducedSet, LinksToParent) ->
    case osm_set:is_member({relation, H}, ReducedSet) of
        false ->
            NewSet = osm_set:add({relation, H}, ReducedSet),
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

filtered_relation(#relation{members = Members} = Relation, Set) ->
    FilteredMembers = lists:filter(fun({T, Id, _}) ->
                                           osm_set:is_member({T, Id}, Set)
                                   end, Members),
    Relation#relation{members = FilteredMembers}.
