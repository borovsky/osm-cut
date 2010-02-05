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
          nodes :: gb_set(),
          ways :: gb_set(),
          relations :: gb_set()
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
    #set{ways = gb_sets:new(),
         nodes = gb_sets:new(),
         relations = gb_sets:new()}.

%%--------------------------------------------------------------------
%% @doc Adds OSM node to set
%% @spec add({node | way | relation, integer()}, #set{}) -> #set{}
%% @end
%%--------------------------------------------------------------------
-spec(add({node | way | relation, integer()}, #set{}) -> #set{}).
add({node, Id}, #set{nodes = Nodes} = Set) ->
    Set#set{nodes = gb_sets:add(Id, Nodes)};

add({way, Id}, #set{ways = Ways} = Set) ->
    Set#set{ways = gb_sets:add(Id, Ways)};

add({relation, Id}, #set{relations = Relations} = Set) ->
    Set#set{relations = gb_sets:add(Id, Relations)}.

%%--------------------------------------------------------------------
%% @doc Checks if OSM node in set
%% @spec is_member({node | way | relation, integer()}, #set{}) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(is_member({node | way | relation, integer()}, #set{}) -> boolean()).
is_member({node, Id}, #set{nodes = Nodes}) ->
    gb_sets:is_member(Id, Nodes);

is_member({way, Id}, #set{ways = Ways}) ->
    gb_sets:is_member(Id, Ways);

is_member({relation, Id}, #set{relations = Relations}) ->
    gb_sets:is_member(Id, Relations).
