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
-define(FLUSH_SIZE, 1000).

%% API
-export([create/1, close/1, add/2, fold/3]).

-record(storage, {
          buffer_size = 0 :: integer(),
          buffer = [] :: list(),
          file,
          file_name
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
create(Name) ->
    {ok, File} = file:open(Name, [read, write, raw, binary]),
    file:truncate(File),
    #storage{file = File, file_name = Name}.

%%--------------------------------------------------------------------
%% @doc Closes node storage. Returns closed storage
%% @spec close(#storage{}) -> #storage{}
%% @end
%%--------------------------------------------------------------------
-spec(close(#storage{}) -> #storage{}).
close(#storage{file = File, file_name = FileName}) ->
    file:close(File),
    file:delete(FileName),
    #storage{}.

%%--------------------------------------------------------------------
%% @doc Adds item to storage
%% @spec add(any(), #storage{}) -> #storage{}
%% @end
%%--------------------------------------------------------------------
-spec(add(any(), #storage{}) -> #storage{}).             
add(Item, #storage{buffer = Buffer,
                   file = File,
                   buffer_size = BufferSize} = Storage) ->
    case BufferSize of
        ?FLUSH_SIZE ->
            Bin = term_to_binary([Item | Buffer]),
            Size = byte_size(Bin),
            file:write(File, [<<Size:32/native>>, Bin]),
            Storage#storage{buffer = [], buffer_size = 0};
        _ ->
            Storage#storage{buffer = [Item | Buffer], buffer_size = BufferSize + 1}
    end.

%%--------------------------------------------------------------------
%% @doc Fold over items
%% @spec fold(fun((any(), any()) -> any()), any(), #storage{}) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(fold(fun((any(), any()) -> any()), any(), #storage{}) -> any()).
fold(Fun, A0, #storage{buffer = Buffer, file = File}) ->
    A1 = lists:foldl(Fun, A0, Buffer),
    file:position(File, bof),
    fold_int(File, Fun, A1).

%%%===================================================================
%%% Internal functions
%%%===================================================================
fold_int(File, Fun, A) ->
    case file:read(File, 4) of
        {ok, <<Size:32/native>>} ->
            {ok, Bin} = file:read(File, Size),
            Term = binary_to_term(Bin),
            A1 = lists:foldl(Fun, A, Term),
            fold_int(File, Fun, A1);
        eof -> A
    end.
