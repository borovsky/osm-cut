%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_set).

%% API
-export([empty/0, add/2, is_member/2]).

-record(set, {
          nodes :: gb_tree(),
          ways :: gb_tree(),
          relations :: gb_tree()
          }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates empty set
%% @spec empty() -> #set{}
%% @end
%%--------------------------------------------------------------------
-spec(empty() -> #set{}).
empty() ->
    #set{ways = osm_bit_set:empty(),
         nodes = osm_bit_set:empty(),
         relations = osm_bit_set:empty()}.

%%--------------------------------------------------------------------
%% @doc Adds OSM node to set
%% @spec add({node | way | relation, integer()}, #set{}) -> #set{}
%% @end
%%--------------------------------------------------------------------
-spec(add({node | way | relation, integer()}, #set{}) -> #set{}).
add({node, Id}, #set{nodes = Nodes} = Set) ->
    Set#set{nodes = osm_bit_set:add(Id, Nodes)};

add({way, Id}, #set{ways = Ways} = Set) ->
    Set#set{ways = osm_bit_set:add(Id, Ways)};

add({relation, Id}, #set{relations = Relations} = Set) ->
    Set#set{relations = osm_bit_set:add(Id, Relations)}.

%%--------------------------------------------------------------------
%% @doc Checks if OSM node in set
%% @spec is_member({node | way | relation, integer()}, #set{}) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(is_member({node | way | relation, integer()}, #set{}) -> boolean()).
is_member({node, Id}, #set{nodes = Nodes}) ->
    osm_bit_set:is_member(Id, Nodes);

is_member({way, Id}, #set{ways = Ways}) ->
    osm_bit_set:is_member(Id, Ways);

is_member({relation, Id}, #set{relations = Relations}) ->
    osm_bit_set:is_member(Id, Relations).
