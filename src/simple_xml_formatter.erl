-module(simple_xml_formatter).

-export([open_to_write/1, formatted_xml/1, formatted_xml/2, open_tag/3, close/1]).

-define(OFFSET, "  ").
open_to_write(FileName) ->
    case file:open(FileName, [write, raw, {delayed_write, 1024000, 2000}]) of
        {ok, Handle} = Return ->
            file:write(Handle, ["<?xml version=\"1.0\"?>"]),
            Return;
        Error -> Error
    end.

close(Handle) ->
    file:close(Handle).
    
formatted_xml(Data) ->
    formatted_xml(Data, 0).

formatted_xml({Tag, Attributes, []}, Offset) ->
    empty_tag(Tag, Attributes, Offset);

formatted_xml({Tag, Attributes, Children}, Offset) ->
    [
     open_tag(Tag, Attributes, Offset),
     [formatted_xml(X, Offset + 1) || X <- Children],
     close_tag(Tag, Offset)
    ].

open_tag(Tag, Attributes, Offset) ->
    ["\n", lists:duplicate(Offset, ?OFFSET),
     "<", Tag, attributes(Attributes), ">"
    ].

close_tag(Tag, Offset) ->
    ["\n", lists:duplicate(Offset, ?OFFSET),
     "</", Tag, ">"
    ].

attributes(Attributes) ->
    [[" ", Name, "=\"", xmerl_lib:export_attribute(Value), $"] ||{Name, Value} <- Attributes].

empty_tag(Tag, Attributes, Offset) ->
    ["\n", lists:duplicate(Offset, ?OFFSET),
     "<", Tag, attributes(Attributes), "/>"].
