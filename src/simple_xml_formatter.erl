%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created : 19 Jan 2010 by Alexander Borovsky <aborovsky@exadel.com>
-module(simple_xml_formatter).

-export([open_to_write/1, formatted_xml/1, formatted_xml/2, open_tag/3, close/1, close_tag/2]).

-include("types.hrl").
-include_lib("kernel/include/file.hrl").

-define(OFFSET, "  ").

%%--------------------------------------------------------------------
%% @doc Opens file and writes XML header
%% @spec open_to_write(string()) -> fd()
%% @end
%%--------------------------------------------------------------------
-spec(open_to_write(string()) -> {error, atom()} | {ok, fd()}).
open_to_write(FileName) ->
    case file:open(FileName, [write, raw, {delayed_write, 1024000, 2000}]) of
        {ok, Handle} = Return ->
            file:write(Handle, ["<?xml version=\"1.0\"?>"]),
            Return;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Closes XML file
%% @spec close(fd()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(close(fd()) -> any()).
close(Handle) ->
    file:close(Handle).

%%--------------------------------------------------------------------
%% @doc Formats XML tag
%% @spec formatted_xml(simple_xml_tag()) -> iolist()
%% @end
%%--------------------------------------------------------------------
-spec(formatted_xml(simple_xml_tag()) -> iolist()).
formatted_xml(Data) ->
    formatted_xml(Data, 0).

%%--------------------------------------------------------------------
%% @doc Formats XML tag with offset
%% @spec formatted_xml(simple_xml_tag(), non_neg_integer()) -> iolist()
%% @end
%%--------------------------------------------------------------------
-spec(formatted_xml(simple_xml_tag(), non_neg_integer()) -> iolist()).
formatted_xml({Tag, Attributes, []}, Offset) ->
    empty_tag(Tag, Attributes, Offset);

formatted_xml({Tag, Attributes, Children}, Offset) ->
    [
     open_tag(Tag, Attributes, Offset),
     [formatted_xml(X, Offset + 1) || X <- Children],
     close_tag(Tag, Offset)
    ].

%%--------------------------------------------------------------------
%% @doc Formats open XML tag
%% @spec open_tag(atom(), attributes(), non_neg_integer()) -> iolist()
%% @end
%%--------------------------------------------------------------------
-spec(open_tag(atom(), attributes(), non_neg_integer()) -> iolist()).
open_tag(Tag, Attributes, Offset) ->
    ["\n", lists:duplicate(Offset, ?OFFSET),
     "<", atom_to_list(Tag), attributes(Attributes), ">"
    ].

%%--------------------------------------------------------------------
%% @doc Formats close XML tag
%% @spec close_tag(atom(), non_neg_integer()) -> iolist()
%% @end
%%--------------------------------------------------------------------
-spec(close_tag(atom(), non_neg_integer()) -> iolist()).
close_tag(Tag, Offset) ->
    ["\n", lists:duplicate(Offset, ?OFFSET),
     "</", atom_to_list(Tag), ">"
    ].

%%--------------------------------------------------------------------
%% @doc Formats attribute list
%% @spec attributes(attributes()) -> iolist()
%% @end
%%--------------------------------------------------------------------
-spec(attributes(attributes()) -> iolist()).
attributes(Attributes) ->
    [[" ", atom_to_list(Name), "=\"", xmerl_lib:export_attribute(osm_utils:ant_to_iolist(Value)), $"] ||{Name, Value} <- Attributes].

%%--------------------------------------------------------------------
%% @doc Formats empty XML tag
%% @spec empty_tag(atom(), attributes(), non_neg_integer()) -> iolist()
%% @end
%%--------------------------------------------------------------------
-spec(empty_tag(atom(), attributes(), non_neg_integer()) -> iolist()).
empty_tag(Tag, Attributes, Offset) ->
    ["\n", lists:duplicate(Offset, ?OFFSET),
     "<", atom_to_list(Tag), attributes(Attributes), "/>"].
