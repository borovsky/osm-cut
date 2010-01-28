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
-export([start_link/1, synchronize/0, close/0, write/1, processing_result/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {out_file, buffer, buffer_size}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Close output file
%% @spec close() -> any()
%% @end
%%--------------------------------------------------------------------
-spec(close() -> any()).
close() ->
    gen_server:call(?SERVER, close).

%%--------------------------------------------------------------------
%% @doc Ping to write server (for avoid message queue overflow)
%% @spec synchronize() -> any()
%% @end
%%--------------------------------------------------------------------
-spec(synchronize() -> any()).
synchronize() ->
    gen_server:call(?SERVER, ping).

%%--------------------------------------------------------------------
%% @doc Writes OSM element to output stream
%% @spec write(source_element()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(write(source_element()) -> any()).
write(endDocument) ->
    gen_server:abcast(?SERVER, endDocument),
    gen_server:call(?SERVER, close);

write(Data) ->
    gen_server:abcast(?SERVER, Data).

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
-spec(init(list(string)) -> {ok, #state{}}).
init([OutputFile]) ->
    {ok, File} = file:open(OutputFile, [write, raw]),
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
-spec(handle_cast(source_element(), #state{}) -> {noreply, #state{}}).
handle_cast(Msg, #state{out_file = OutFile} = State) ->
    Xml = xml_for_element(Msg),
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
xml_for_element({osm, _Attributes, []} = Element) ->
    formatted_open_tag(Element);

xml_for_element(endDocument) ->
    formatted_close_tag(osm);

xml_for_element({node, Id, {X, Y}, Attributes, Tags}) ->
    AllAttributes = [{id, Id}, {lon, X}, {lat, Y} | Attributes],
    TagXml = [{tag, [{k, K}, {v, V}], []} || {K, V} <- Tags],
    formatted_xml({node, AllAttributes, TagXml});

xml_for_element({way, Id, Nodes, Attributes, Tags}) ->
    AllAttributes = [{id, Id} | Attributes],
    TagXml = [{tag, [{k, K}, {v, V}], []} || {K, V} <- Tags],
    ChildrenXml = way_tags_with_nodes(Nodes, TagXml),
    formatted_xml({way, AllAttributes, ChildrenXml});

xml_for_element({relation, Id, Members, Attributes, Tags}) ->
    AllAttributes = [{id, Id} | Attributes],
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
    simple_xml_formatter:formatted_xml(Element, 1).

formatted_open_tag({Tag, Attributes, _Children}) ->
    simple_xml_formatter:open_tag(Tag, Attributes, 0).

formatted_close_tag(Tag) ->
    simple_xml_formatter:close_tag(Tag, 0).

