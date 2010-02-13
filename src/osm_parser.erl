%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-module(osm_parser).

-export([parse/1]).

-record(event_state, {
          level = -1,
          tag_stack = [],
          children_stack = [],
          processor_state = osm_processor:init_client():: tuple()
         }).

-include("../include/types.hrl").

-spec(parse(string()) -> any()).
parse(SourceFile) ->
    Start = erlang:now(),

    {ok, File} = file:open(SourceFile, [read, raw, binary]),
    erlsom:parse_sax(<<>>, #event_state{}, fun sax_callback/2,
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

process_element({osm, _, _} = Element, #event_state{processor_state = ProcessorState} = State) ->
    NewProcState = osm_processor:process(Element, ProcessorState),
    State#event_state{processor_state = NewProcState};

process_element({node, Attributes, Children},
                #event_state{processor_state = ProcessorState} = State) ->
    Node = populate_node_attributes(#node{tags = encoded_tags(Children)}, Attributes),
    NewProcState = osm_processor:process(Node, ProcessorState),
    State#event_state{processor_state = NewProcState};

process_element({way, Attributes, Children},
                #event_state{processor_state = ProcessorState} = State) ->

    {Nodes, Tags} = classified_way_children(Children),
    Way = populate_way_attributes(#way{tags = encoded_tags(Tags), nodes = Nodes}, Attributes),
    
    NewProcState = osm_processor:process(Way, ProcessorState),
    State#event_state{processor_state = NewProcState};

process_element({relation, Attributes, Children},
                #event_state{processor_state = ProcessorState} = State) ->
    {Members, Tags} = classified_relation_children(Children),

    Relation = populate_relation_attributes(#relation{tags = encoded_tags(Tags), members = Members}, Attributes),
    
    NewProcState = osm_processor:process(Relation, ProcessorState),
    State#event_state{processor_state = NewProcState};

process_element(endDocument, #event_state{processor_state = ProcessorState} = State) ->
    NewProcState = osm_processor:process(endDocument, ProcessorState),
    State#event_state{processor_state = osm_processor:synchronize(NewProcState)};

process_element(Element, State) ->
    io:format("Unprocessed element: ~p~n", [Element]),
    State.

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

-spec(simple_xml(tuple(), list(simple_xml_tag)) -> simple_xml_tag()).
simple_xml({startElement, _, Tag, _, Attributes}, Children) ->
    {list_to_atom(Tag), % Risky, but fast
     lists:map(fun({attribute, Name, _, _, Value}) -> {list_to_atom(Name), Value} end, Attributes),
     Children}.

populate_node_attributes(#node{} = Node, [{lon, Lon} | Tail]) ->
    populate_node_attributes(Node#node{x = osm_utils:to_float(Lon)}, Tail);

populate_node_attributes(#node{} = Node, [{lat, Lat} | Tail]) ->
    populate_node_attributes(Node#node{y = osm_utils:to_float(Lat)}, Tail);

%Common attributes
populate_node_attributes(#node{} = Node, [{id, Id} | Tail]) ->
    populate_node_attributes(Node#node{id = list_to_integer(Id)}, Tail);

populate_node_attributes(#node{} = Node, [{version, Version} | Tail]) ->
    populate_node_attributes(Node#node{version = list_to_integer(Version)}, Tail);

populate_node_attributes(#node{} = Node, [{changeset, Changeset} | Tail]) ->
    populate_node_attributes(Node#node{changeset = list_to_integer(Changeset)}, Tail);

populate_node_attributes(#node{} = Node, [{uid, Uid} | Tail]) ->
    populate_node_attributes(Node#node{uid = list_to_integer(Uid)}, Tail);

populate_node_attributes(#node{} = Node, [{user, User} | Tail]) ->
    populate_node_attributes(Node#node{user = osm_utils:list_to_atom_or_binary(User)}, Tail);

populate_node_attributes(#node{} = Node, [{timestamp, Timestamp} | Tail]) ->
    populate_node_attributes(Node#node{timestamp = unicode:characters_to_binary(Timestamp)}, Tail);

populate_node_attributes(#node{} = Node, [Attr | Tail]) ->
    io:format("Unknown attribute: ~p~n", [Attr]),
    populate_node_attributes(Node, Tail);

populate_node_attributes(#node{} = Node, []) ->
    Node.

populate_way_attributes(#way{} = Way, [{id, Id} | Tail]) ->
    populate_way_attributes(Way#way{id = list_to_integer(Id)}, Tail);

populate_way_attributes(#way{} = Way, [{version, Version} | Tail]) ->
    populate_way_attributes(Way#way{version = list_to_integer(Version)}, Tail);

populate_way_attributes(#way{} = Way, [{changeset, Changeset} | Tail]) ->
    populate_way_attributes(Way#way{changeset = list_to_integer(Changeset)}, Tail);

populate_way_attributes(#way{} = Way, [{uid, Uid} | Tail]) ->
    populate_way_attributes(Way#way{uid = list_to_integer(Uid)}, Tail);

populate_way_attributes(#way{} = Way, [{user, User} | Tail]) ->
    populate_way_attributes(Way#way{user = osm_utils:list_to_atom_or_binary(User)}, Tail);

populate_way_attributes(#way{} = Way, [{timestamp, Timestamp} | Tail]) ->
    populate_way_attributes(Way#way{timestamp = unicode:characters_to_binary(Timestamp)}, Tail);

populate_way_attributes(#way{} = Way, [Attr | Tail]) ->
    io:format("Unknown attribute: ~p~n", [Attr]),
    populate_way_attributes(Way, Tail);

populate_way_attributes(#way{} = Way, []) ->
    Way.

populate_relation_attributes(#relation{} = Relation, [{id, Id} | Tail]) ->
    populate_relation_attributes(Relation#relation{id = list_to_integer(Id)}, Tail);

populate_relation_attributes(#relation{} = Relation, [{version, Version} | Tail]) ->
    populate_relation_attributes(Relation#relation{version = list_to_integer(Version)}, Tail);

populate_relation_attributes(#relation{} = Relation, [{changeset, Changeset} | Tail]) ->
    populate_relation_attributes(Relation#relation{changeset = list_to_integer(Changeset)}, Tail);

populate_relation_attributes(#relation{} = Relation, [{uid, Uid} | Tail]) ->
    populate_relation_attributes(Relation#relation{uid = list_to_integer(Uid)}, Tail);

populate_relation_attributes(#relation{} = Relation, [{user, User} | Tail]) ->
    populate_relation_attributes(Relation#relation{user = osm_utils:list_to_atom_or_binary(User)}, Tail);

populate_relation_attributes(#relation{} = Relation, [{timestamp, Timestamp} | Tail]) ->
    populate_relation_attributes(Relation#relation{timestamp = unicode:characters_to_binary(Timestamp)}, Tail);

populate_relation_attributes(#relation{} = Relation, [Attr | Tail]) ->
    io:format("Unknown attribute: ~p~n", [Attr]),
    populate_relation_attributes(Relation, Tail);

populate_relation_attributes(#relation{} = Relation, []) ->
    Relation.
