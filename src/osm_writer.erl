%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(osm_writer).

-include("types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, synchronize/1, init_client/0, write/2, processing_result/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {out_file, buffer, buffer_size}).

-record(writer_buffer, {
          buffer_size = 0 :: non_neg_integer(),
          buffer = [] :: list(source_element())
         }).
-define(WRITING_SIZE, 1000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes writer's client structure
%% @spec init_client() -> #writer_buffer{}
%% @end
%%--------------------------------------------------------------------
-spec(init_client() -> #writer_buffer{}).
init_client() ->
    #writer_buffer{}.

%%--------------------------------------------------------------------
%% @doc Close output file
%% @spec close(#writer_buffer{}) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(close(#writer_buffer{}) -> #writer_buffer{}).
close(State) ->
    R = synchronize(State),
    gen_server:call(?SERVER, close, infinity),
    R.

%%--------------------------------------------------------------------
%% @doc Ping to write server (for avoid message queue overflow)
%% @spec synchronize(#writer_buffer{}) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(synchronize(#writer_buffer{}) -> #writer_buffer{}).
synchronize(#writer_buffer{buffer = Buffer}) ->
    gen_server:abcast(?SERVER, Buffer), % We send reversed buffer, map will reverse it
    gen_server:call(?SERVER, ping, infinity),
    #writer_buffer{}.

%%--------------------------------------------------------------------
%% @doc Writes OSM element to output stream
%% @spec write(#writer_buffer{}, source_element()) -> #writer_buffer{}
%% @end
%%--------------------------------------------------------------------
-spec(write(#writer_buffer{}, source_element()) -> #writer_buffer{}).
write(State, endDocument) ->
    B1 = do_write(State, endDocument),
    close(B1);

write(State, Data) ->
    do_write(State, Data).

do_write(#writer_buffer{buffer_size = BufferSize,
                        buffer = Buffer} = Buf, Node) ->
    case BufferSize of
        ?WRITING_SIZE ->
            gen_server:abcast(?SERVER, [Node | Buffer]),
            #writer_buffer{};
        _ ->
            Buf#writer_buffer{buffer = [Node | Buffer], buffer_size = BufferSize + 1}
    end.


%%--------------------------------------------------------------------
%% @doc Returns processing result
%% @spec processing_result() -> ok
%% @end
%%--------------------------------------------------------------------
-spec(processing_result() -> ok).
processing_result() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(string()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec(start_link(string()) -> any()).
start_link(Options) ->
    OutputFile = proplists:get_value(output_file, Options),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [OutputFile], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(list(string())) -> {ok, #state{}}).
init([OutputFile]) ->
    {ok, File} = osm_simple_xml_formatter:open_to_write(OutputFile),
    {ok, #state{out_file = File}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(ping | close, pid(), #state{}) -> {reply, pong | ok, #state{}}).
handle_call(ping, _From, State) ->
    {reply, pong, State};

handle_call(close, _From, #state{out_file = OutFile} = State) ->
    file:close(OutFile),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(list(source_element()), #state{}) -> {noreply, #state{}}).
handle_cast(Msg, #state{out_file = OutFile} = State) ->
    Xml = lists:foldl(fun(M, S) -> [xml_for_element(M), S] end, [], Msg),
    file:write(OutFile, Xml),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(any(), #state{}) -> {noreply, #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(any(), #state{}) -> any()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(any(), #state{}, any()) -> {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(xml_for_element(source_element()) -> iolist()).
xml_for_element({osm, _Attributes, []} = Element) ->
    formatted_open_tag(Element);

xml_for_element(endDocument) ->
    formatted_close_tag(osm);

xml_for_element(#node{id = Id,
                      x = X,
                      y = Y,
                      version = Version,
                      timestamp = Timestamp,
                      uid = Uid,
                      changeset = Changeset,
                      user = User,
                      tags = Tags}) ->
    AllAttributes = [{id, Id}, {lon, X}, {lat, Y},
                     {version, Version}, {timestamp, Timestamp},
                     {uid, Uid}, {changeset, Changeset},
                     {user, User}],
    TagXml = [{tag, [{k, K}, {v, V}], []} || {K, V} <- Tags],
    formatted_xml({node, AllAttributes, TagXml});

xml_for_element(#way{id=Id,
                     nodes = Nodes,
                     version = Version,
                     timestamp = Timestamp,
                     uid = Uid,
                     changeset = Changeset,
                     user = User,
                     tags = Tags}) ->
    AllAttributes = [{id, Id},
                     {version, Version}, {timestamp, Timestamp},
                     {uid, Uid}, {changeset, Changeset},
                     {user, User}],
    TagXml = [{tag, [{k, K}, {v, V}], []} || {K, V} <- Tags],
    ChildrenXml = way_tags_with_nodes(Nodes, TagXml),
    formatted_xml({way, AllAttributes, ChildrenXml});

xml_for_element(#relation{id = Id,
                          members = Members,
                          version = Version,
                          timestamp = Timestamp,
                          uid = Uid,
                          changeset = Changeset,
                          user = User,
                          tags = Tags}) ->
    AllAttributes = [{id, Id},
                     {version, Version}, {timestamp, Timestamp},
                     {uid, Uid}, {changeset, Changeset},
                     {user, User}],
    TagXml = [{tag, [{k, K}, {v, V}], []} || {K, V} <- Tags],
    ChildrenXml = relation_tags_with_members(Members, TagXml),
    formatted_xml({relation, AllAttributes, ChildrenXml}).

way_tags_with_nodes([], Children) ->
    Children;

way_tags_with_nodes([H | T], Children) ->
    way_tags_with_nodes(T, [{nd, [{ref, H}], []} | Children]). 

relation_tags_with_members([], Children) ->
    Children;

relation_tags_with_members([{Type, Id, Role} | T], Children) ->
    relation_tags_with_members(T, [{member, [{type, Type}, {id, Id}, {role, Role}], []} | Children]).

formatted_xml(Element) ->
    osm_simple_xml_formatter:formatted_xml(Element, 1).

formatted_open_tag({Tag, Attributes, _Children}) ->
    osm_simple_xml_formatter:open_tag(Tag, Attributes, 0).

formatted_close_tag(Tag) ->
    osm_simple_xml_formatter:close_tag(Tag, 0).

