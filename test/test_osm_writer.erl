%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(test_osm_writer).

-include("types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, synchronize/1, close/0, write/2, processing_result/0, init_client/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {nodes = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes writer's client structure
%% @spec init_client() -> #writer_buffer{}
%% @end
%%--------------------------------------------------------------------
-spec(init_client() -> ok).
init_client() ->
    ok.

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
-spec(synchronize(any()) -> any()).
synchronize(_) ->
    gen_server:call(?SERVER, ping).

%%--------------------------------------------------------------------
%% @doc Writes OSM element to output stream
%% @spec write(source_element()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(write(any(), source_element()) -> any()).
write(_, endDocument) ->
    gen_server:abcast(?SERVER, endDocument),
    gen_server:call(?SERVER, close);

write(_, Data) ->
    gen_server:abcast(?SERVER, Data).

%%--------------------------------------------------------------------
%% @doc Returns processing result
%% @spec processing_result() -> ok
%% @end
%%--------------------------------------------------------------------
-spec(processing_result() -> ok).
processing_result() ->
    gen_server:call(?SERVER, get_nodes).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(string()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec(start_link(string()) -> any()).
start_link(_Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{nodes = []}}.

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

handle_call(close, _From, State) ->
    {reply, ok, State};

handle_call(get_nodes, _From, #state{nodes = Nodes} = State) ->
    {reply, Nodes, State};

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
handle_cast(Msg, #state{nodes = Nodes} = State) ->
    {noreply, State#state{nodes = [Msg | Nodes]}}.

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
