%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_utils).

%% API
-export([to_float/1, any_to_iolist/1, any_to_bin/1, list_to_atom_or_binary/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Parses floats and integers (default to_float can't parse integer)
%% @spec to_float(string()) -> float() | integer()
%% @end
%%--------------------------------------------------------------------
-spec(to_float(string()) -> float() | integer()).
to_float(S) ->
    try list_to_float(S) of
        F -> F
    catch
        error:badarg ->
            list_to_integer(S)
    end.

%% Generates text (iolist) representation
-spec(any_to_iolist(any()) -> binary()).             
any_to_iolist(A) when is_atom(A) ->
    atom_to_list(A);
any_to_iolist(A) when is_integer(A) ->
    integer_to_list(A);
any_to_iolist(A) when is_float(A) ->
    float_to_list(A);
any_to_iolist(A) when is_boolean(A) ->
    case A of
        true -> "true";
        false -> "false"
    end;
any_to_iolist(A) ->
    A.


%% Generates common (binary) representation (for check if items equal)
-spec(any_to_bin(any()) -> binary()).             
any_to_bin(A) ->
    iolist_to_binary(any_to_iolist(A)).

-spec(list_to_atom_or_binary(string()) -> binary()).
list_to_atom_or_binary("") ->
    <<>>;

list_to_atom_or_binary(S) ->
    try list_to_atom(S) of
        A -> A
    catch
        _:_ -> unicode:characters_to_binary(S)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
