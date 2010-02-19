%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_bit_set).

%% API
-export([empty/0, add/2, is_member/2]).

%%%===================================================================
%%% API
%%%===================================================================

-define(BITS_PER_INTEGER, 512).

%%--------------------------------------------------------------------
%% @doc Creates empty set
%% @spec empty() -> gb_tree()
%% @end
%%--------------------------------------------------------------------
-spec(empty() -> gb_tree()).
empty() ->
    gb_trees:empty().

%%--------------------------------------------------------------------
%% @doc Adds integer to set
%% @spec add(integer(), gb_tree()) -> gb_tree()
%% @end
%%--------------------------------------------------------------------
-spec(add(integer(), gb_tree()) -> gb_tree()).
add(Id, Array) ->
    Idx = Id div ?BITS_PER_INTEGER,
    Bit = Id rem ?BITS_PER_INTEGER,
    Val = case gb_trees:lookup(Idx, Array) of
              none -> 0;
              {value, V} -> V
          end,
    gb_trees:enter(Idx, Val bor (1 bsl Bit), Array).

%%--------------------------------------------------------------------
%% @doc Checks if integer in set
%% @spec is_member(integer(), gb_tree()) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(is_member(integer(), gb_tree()) -> boolean()).
is_member(Id, Array) ->
    Idx = Id div ?BITS_PER_INTEGER,
    Bit = Id rem ?BITS_PER_INTEGER,
    Val = case gb_trees:lookup(Idx, Array) of
              none -> 0;
              {value, V} -> V
          end,
    Val band (1 bsl Bit) > 0.
