%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(osm_parser).

-export([parse/2]).

-record(event_state, {
          level = -1,
          tag_stack = [],
          children_stack = [],
          message_count = 0,
          processor_module
         }).

-include("types.hrl").

-spec(parse(string(), property_list()) -> any()).
parse(SourceFile, Options) ->
    ProcessorModule = proplists:get_value(processor_module, Options, osm_processor),
    Start = erlang:now(),

    {ok, File} = file:open(SourceFile, [read, raw, binary]),
    erlsom:parse_sax(<<>>, #event_state{processor_module = ProcessorModule}, fun sax_callback/2,
                     [{continuation_function, fun continuation_reader/2, File}]),
    End = erlang:now(),
    io:format("Parse time: ~p~n", [timer:now_diff(End, Start)]).


continuation_reader(Tail, File) ->
    case file:read(File, 100000) of
        {ok, Data} ->
            {<<Tail/binary, Data/binary>>, File};
        eof -> {Tail, File}
    end.

sax_callback(startDocument, State) ->
    State#event_state{level=0};

sax_callback({processingInstruction, _, _}, State) ->
    State;

sax_callback({startElement, _,"osm", _, _Attributes} = Element, State) ->
    Osm = simple_xml(Element, []),
    process_element(Osm, State#event_state{level=1});

sax_callback({ignorableWhitespace, _}, State) ->
    State;

sax_callback({startElement, _, _ElementName, _, _Attributes} = Element,
             #event_state{level = Level,
                          tag_stack = TagStack,
                          children_stack = ChildrenStack} = State) ->
    State#event_state{level = Level + 1,
                      tag_stack = [Element | TagStack],
                      children_stack = [[] | ChildrenStack]
                     };

sax_callback({endElement, _, _ElementName, _},
             #event_state{level = Level,
                          tag_stack = [RootElement | TagStack],
                          children_stack = [RootChildren | ChildrenStack]} = State) ->
    Element = simple_xml(RootElement, RootChildren),

    case Level of
        2 ->
            NewState = process_element(Element, State),
            NewState#event_state{level = 1,
                                 tag_stack = [],
                                 children_stack = []
                                };
        _ ->
            [ParentChildren | OtherChildren] = ChildrenStack,
            State#event_state{level = Level - 1,
                              tag_stack = TagStack,
                              children_stack = [[Element | ParentChildren] | OtherChildren]
                             }
    end;
sax_callback({endElement, _, "osm", _}, State) ->
    State;

sax_callback(endDocument, State) ->
    process_element(endDocument, State);

sax_callback(Event, State) ->
    io:format("Unprocessed: ~p~n    ~p~n", [Event, State]),
    State.

process_element({osm, _, _} = Element, State) ->
    send_message(Element, State#event_state.processor_module, 1),
    State;

process_element({node, Attributes, Children},
                #event_state{message_count = MessageCount,
                             processor_module = Processor} = State) ->
    Lon = osm_utils:to_float(proplists:get_value(lon, Attributes)),
    Lat = osm_utils:to_float(proplists:get_value(lat, Attributes)),
    Id = list_to_integer(proplists:get_value(id, Attributes)),
    send_message({node, Id, {Lon, Lat}, strip_attributes(Attributes, [id, lat, lon]),
                 encoded_tags(Children)}, Processor, MessageCount),
    State#event_state{message_count = MessageCount + 1};

process_element({way, Attributes, Children},
                #event_state{message_count = MessageCount,
                             processor_module = Processor} = State) ->
    {Nodes, Tags} = classified_way_children(Children),
    Id = list_to_integer(proplists:get_value(id, Attributes)),
    
    send_message({way, Id, Nodes, strip_attributes(Attributes, [id]),
                 encoded_tags(Tags)}, Processor, MessageCount),
    State#event_state{message_count = MessageCount + 1};

process_element({relation, Attributes, Children},
                #event_state{message_count = MessageCount,
                             processor_module = Processor} = State) ->
    {Members, Tags} = classified_relation_children(Children),
    Id = list_to_integer(proplists:get_value(id, Attributes)),
    
    send_message({relation, Id, Members, strip_attributes(Attributes, [id]),
                 encoded_tags(Tags)}, Processor, MessageCount),
    State#event_state{message_count = MessageCount + 1};

process_element(endDocument, State) ->
    send_message(endDocument, State#event_state.processor_module, 0), % with synchronization
    State;

process_element(Element, State) ->
    io:format("Unprocessed element: ~p~n", [Element]),
    State.

-spec(send_message(source_element(), atom(), integer()) -> any()).
send_message(Element, Processor, MessageCount) ->
    Processor:process(Element),
    case MessageCount rem 10000 of
        0 -> Processor:synchronize();
        _ -> ok
    end.

-spec(classified_way_children(simple_xml_tags()) -> {list(integer()), simple_xml_tags()}).
classified_way_children(Children) ->
    classified_way_children(Children, [], []).

-spec(classified_way_children(simple_xml_tags(), list(integer()), simple_xml_tags()) ->
             {list(integer()), simple_xml_tags()}).
classified_way_children([{nd, [{ref, Ref}], []} | Children], RefNodes, OtherChildren) ->
    classified_way_children(Children, [list_to_integer(Ref) | RefNodes], OtherChildren);

classified_way_children([Other | Children], RefNodes, OtherChildren) ->
    classified_way_children(Children, RefNodes, [Other | OtherChildren]);

classified_way_children([], RefNodes, OtherChildren) ->
    {RefNodes, OtherChildren}.

-spec(classified_relation_children(simple_xml_tags()) -> {members(), simple_xml_tags()}).
classified_relation_children(Children) ->
    classified_relation_children(Children, [], []).

-spec(classified_relation_children(simple_xml_tags(), members(), simple_xml_tags()) ->
             {members(), simple_xml_tags()}).
classified_relation_children([{member, Attributes, []} | Children], RefNodes, OtherChildren) ->
    Type = list_to_atom(proplists:get_value(type, Attributes)),
    Ref = list_to_integer(proplists:get_value(ref, Attributes)),
    Role = osm_utils:list_to_atom_or_binary(proplists:get_value(role, Attributes)),
    
    classified_relation_children(Children, [{Type, Ref, Role} | RefNodes], OtherChildren);

classified_relation_children([Other | Children], RefNodes, OtherChildren) ->
    classified_relation_children(Children, RefNodes, [Other | OtherChildren]);

classified_relation_children([], RefNodes, OtherChildren) ->
    {RefNodes, OtherChildren}.

-spec(encoded_tags(simple_xml_tags()) -> tags()).
encoded_tags(List) ->
    encoded_tags(List, []).

-spec(encoded_tags(simple_xml_tags(), tags()) -> tags()).
encoded_tags([{tag, Attributes, []} | Tail], List) ->
    K = osm_utils:list_to_atom_or_binary(proplists:get_value(k, Attributes)),
    V = unicode:characters_to_binary(proplists:get_value(v, Attributes)),
    encoded_tags(Tail, [{K, V} | List]);

encoded_tags([], List) ->
    List.

% Removes extra attributes, optimize other
-spec(strip_attributes(attributes(), list(atom())) -> attributes()).
strip_attributes(Attributes, Strip) ->
    strip_attributes(Attributes, [], Strip).

strip_attributes([], ProcessedAttributes, _) ->
    ProcessedAttributes;

strip_attributes([{K, V} | T], ProcessedAttributes, Strip) ->
    case proplists:get_bool(K, Strip) of
        true -> strip_attributes(T, ProcessedAttributes, Strip);
        _ ->
            strip_attributes(T, [optimize_attribute(K, V) | ProcessedAttributes], Strip)
    end. 

-spec(optimize_attribute(atom(), list()) -> attribute()).
optimize_attribute(changeset = K, V) ->
    {K, list_to_integer(V)};

%We have not so many users for overflow atoms table
optimize_attribute(user = K, V) ->
    {K, osm_utils:list_to_atom_or_binary(V)};

optimize_attribute(uid = K, V) ->
    {K, list_to_integer(V)};

optimize_attribute(version = K, V) ->
    {K, list_to_integer(V)};

optimize_attribute(timestamp = K, V) ->
    {K, unicode:characters_to_binary(V)};

optimize_attribute(K, V) ->
    {K, V}.

-spec(simple_xml(tuple(), list(simple_xml_tag)) -> simple_xml_tag()).
simple_xml({startElement, _, Tag, _, Attributes}, Children) ->
    {list_to_atom(Tag), % Risky, but fast
     lists:map(fun({attribute, Name, _, _, Value}) -> {list_to_atom(Name), Value} end, Attributes),
     Children}.
