%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2010 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(osm_processor).

-behaviour(gen_server).

-include("types.hrl").

%% API
-export([start_link/1, init_client/0, process/2, synchronize/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

-define(SERVER, ?MODULE).

-record(state, {
          processor_module,
          processor_state,
          writer_module
         }).

-record(processor_buffer, {
          buffer_size = 0 :: non_neg_integer(),
          buffer = [] :: list(source_element()),
          sends_count = 0:: non_neg_integer()
         }).
-define(PROCESSING_SIZE, 100).
-define(SENDS_BETWEEN_SYNC, 10).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes processor's client structure
%% @spec init_client() -> #processor_buffer{}
%% @end
%%--------------------------------------------------------------------
-spec(init_client() -> #processor_buffer{}).
init_client() ->
    #processor_buffer{}.

%%--------------------------------------------------------------------
%% @doc Processes OSM element
%% @spec process(source_element(), #processor_buffer{}) -> #processor_buffer{}
%% @end
%%--------------------------------------------------------------------
-spec(process(source_element(), #processor_buffer{}) -> #processor_buffer{}).
process(Node, #processor_buffer{buffer_size = BufferSize,
                                buffer = Buffer,
                                sends_count = SendsCount} = Buf) ->
    case BufferSize of
        ?PROCESSING_SIZE ->
            ToSend = lists:reverse([Node | Buffer]),
            case SendsCount of %We should synchronize sometimes for avoid message query overflow
                ?SENDS_BETWEEN_SYNC ->
                    gen_server:call(?SERVER, ping, infinity),
                    gen_server:abcast(?SERVER, ToSend),
                    #processor_buffer{};
                _ ->
                    gen_server:abcast(?SERVER, ToSend),
                    #processor_buffer{sends_count = SendsCount + 1}
            end;
        _ ->
            Buf#processor_buffer{buffer = [Node | Buffer], buffer_size = BufferSize + 1}
    end.

%%--------------------------------------------------------------------
%% @doc Synchronize processor with parser (for avoid message query overflow)
%% @spec synchronize(#processor_buffer{}) -> #processor_buffer{}
%% @end
%%--------------------------------------------------------------------
-spec(synchronize(#processor_buffer{}) -> #processor_buffer{}).
synchronize(#processor_buffer{buffer = Buffer}) ->
    gen_server:abcast(?SERVER, lists:reverse(Buffer)),
    gen_server:call(?SERVER, ping, infinity),
    #processor_buffer{}.


%%--------------------------------------------------------------------
%% @doc Interface for processing modules
%% @spec behaviour_info(atom()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(behaviour_info(atom()) -> any()).
behaviour_info(callbacks) ->
    [{init, 1},
     {process_message, 2}];
behaviour_info(_Other) ->
    undefined.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(property_list()) -> tuple() | ignore
%% @end
%%--------------------------------------------------------------------
-spec(start_link(property_list()) -> tuple() | ignore).
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init([any()]) -> {ok, #state{}} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init([any()]) -> {ok, #state{}}).
init(Options) ->
    CompleteObjects = proplists:get_bool(complete_objects, Options),

    ProcessorModule = case CompleteObjects of
                          false -> osm_process_non_complete;
                          _ -> osm_process_complete
                      end,
    ProcessorState = ProcessorModule:init(Options),
    WriterModule = proplists:get_value(writer_module, Options, osm_writer),
    
    {ok, #state{processor_module = ProcessorModule,
                processor_state = ProcessorState,
                writer_module = WriterModule}}.

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
-spec(handle_call(ping, pid(), #state{}) -> {reply, pong, #state{}}).
handle_call(ping, _From, #state{writer_module = Writer} = State) ->
    Writer:synchronize(),
    {reply, pong, State};

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
handle_cast(Msg, #state{processor_module = ProcessorModule,
                        processor_state = ProcessorState} = State) ->
    NewState = lists:foldl(fun(E, S) -> ProcessorModule:process_message(E, S) end,
                           ProcessorState,
                           Msg),
    {noreply, State#state{processor_state = NewState}}.

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
-spec(handle_info(any, #state{}) -> {noreply, #state{}}).
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

