%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_node_storage).

-include("types.hrl").

%% API
-export([create/1, close/1, add/2, fold/3]).

-record(storage, {
          buffer = [] :: list()
          }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates new node storage
%% @spec create(string()) -> #storage{}
%% @end
%%--------------------------------------------------------------------
-spec(create(string()) -> #storage{}).
create(_Name) ->
    #storage{}.

%%--------------------------------------------------------------------
%% @doc Closes node storage. Returns closed storage
%% @spec close(#storage{}) -> #storage{}
%% @end
%%--------------------------------------------------------------------
-spec(close(#storage{}) -> #storage{}).
close(#storage{}) ->
    #storage{}.

%%--------------------------------------------------------------------
%% @doc Adds item to storage
%% @spec add(any(), #storage{}) -> #storage{}
%% @end
%%--------------------------------------------------------------------
-spec(add(any(), #storage{}) -> #storage{}).             
add(Item, #storage{buffer = Buffer}) ->
    #storage{buffer = [Item | Buffer]}.

%%--------------------------------------------------------------------
%% @doc Fold over items
%% @spec fold(fun((any(), any()) -> any()), any(), #storage{}) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(fold(fun((any(), any()) -> any()), any(), #storage{}) -> any()).
fold(Fun, A0, #storage{buffer = Buffer}) ->
    lists:foldl(Fun, A0, Buffer).

%%%===================================================================
%%% Internal functions
%%%===================================================================
