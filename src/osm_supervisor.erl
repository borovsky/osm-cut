%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <aborovsky@exadel.com>
%%% @copyright (C) 2010, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2010 by Alexander Borovsky <aborovsky@exadel.com>
%%%-------------------------------------------------------------------
-module(osm_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("types.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec(start_link(property_list()) -> {ok, pid()} | ignore | {error, any()}).
start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(list(property_list())) -> {ok, {tuple(), list(tuple())}}).
init(Options) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},


    ProcessorChild = processor_child(Options),
    WriterChild = writer_child(Options),

    {ok, {SupFlags, [ProcessorChild, WriterChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates supervisor child struct for processor server
%% @spec processor_child(property_list()) -> tuple()
%% @end
%%--------------------------------------------------------------------
-spec(processor_child(property_list()) -> tuple()).
processor_child(Options) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {processor, {osm_processor, start_link, [Options]},
              Restart, Shutdown, Type, [osm_processor]}.

%%--------------------------------------------------------------------
%% @doc Creates supervisor child struct for write server
%% @spec writer_child(property_list()) -> tuple()
%% @end
%%--------------------------------------------------------------------
-spec(writer_child(property_list()) -> tuple()).
writer_child(Options) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    WriterModule = proplists:get_value(writer_module, Options, osm_writer),

    {writer, {WriterModule, start_link, [Options]},
              Restart, Shutdown, Type, [WriterModule]}.

