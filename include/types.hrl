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

-type(interval() :: {{float(), float(), float()}, float(), float()} | {float(), float(), float()}).

-type(property() :: tuple() | atom()).
-type(property_list() :: list(property())).

-type(attribute() :: {atom(), atom() | integer() | binary()}).
-type(attributes() :: list(attribute())).

-type(simple_xml_tag() :: {atom(), attributes(), list(tuple())}).
-type(simple_xml_tags() :: list(simple_xml_tag())).

-type(tag() :: {binary(), binary()}).
-type(tags() :: list(tag())).
-type(member() :: {node | way | relation, integer(), binary()}).
-type(members() :: list(member())).

-type(root_element() :: {osm, attributes()}).

-record(osm, {
          attributes :: attributes(),
          childs :: []
          }).
-record(node,
        {
          id = 0:: integer(),
          position :: point(),
          attributes :: attributes(),
          tags :: tags()
         }).
-record(way, {
          id = 0 :: integer(),
          nodes :: list(integer()),
          attributes :: attributes(),
          tags :: tags()
         }).

-record(relation, {
          id = 0 :: integer(),
          members :: members(),
          attributes :: attributes(),
          tags :: tags()
          }).

-type(source_element() ::
      #osm{} |
      #node{} |
      #way{} |
      #relation{} |
      endDocument).

-type(osm_set() :: {set, gb_set(), gb_set(), gb_set()}).
