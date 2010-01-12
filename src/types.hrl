%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>

-type(point() :: {float(), float()}).

-type(polygon_def() :: {include | exclude, [point()]}).
-type(polygon_list() :: list(polygon_def())).

-type(polygon_function() ::fun((float(), float()) -> boolean())).


-type(interval() :: {float(), float(), float()}).

-type(property() :: tuple() | atom()).
-type(property_list() :: list(property())).

-type(attribute() :: {atom(), binary()}).
-type(attributes() :: list(attribute())).

-type(simple_xml_tag() :: {atom(), attributes(), list(tuple())}).
-type(simple_xml_tags() :: list(simple_xml_tag())).

-type(tag() :: {binary(), binary()}).
-type(tag_list() :: list(tag())).
-type(member() :: {node | way | relation, integer(), binary()}).
-type(members() :: list(member())).

-type(root_element() :: {osm, attributes(),[]}).
-type(node_element() :: {node, integer(), point(), attributes(), tag_list()}).
-type(way_element() :: {way, integer(), list(integer()), attributes(), tag_list()}).
-type(relation_element() :: {relation, integer(), members(), attributes(), tag_list()}).

-type(source_element() ::
      root_element() |
      node_element() |
      way_element() |
      relation_element() |
      endDocument).
