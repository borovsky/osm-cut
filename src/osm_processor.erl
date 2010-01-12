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
-export([start_link/1, process/1, synchronize/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
         polygon_function,
         reduced_set
         }).

%%%===================================================================
%%% API
%%%===================================================================

-spec(process(source_element()) -> any()).
process(Node) ->
    gen_server:abcast(?SERVER, Node).

synchronize() ->
    gen_server:call(?SERVER, ping).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(fun((float(), float()) -> boolean())) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec(start_link(polygon_function()) -> tuple() | ignore).
start_link(PolygonFunction) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [PolygonFunction], []).

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
-spec(init([polygon_function()]) -> {ok, #state{}}).
init([PolygonFunction]) ->
    {ok, #state{polygon_function = PolygonFunction, reduced_set = gb_sets:new()}}.

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
handle_call(ping, _From, State) ->
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
handle_cast(Msg, State) ->
    {noreply, process_message(Msg, State)}.

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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(process_message(source_element(), #state{}) -> #state{}).
process_message({osm, _Attributes, []} = Element, State) ->
    osm_writer:write(Element),
    State;

process_message(endDocument, State) ->
    osm_writer:write(endDocument),
    State;

process_message({node, Id, {X, Y}, _, _} = Element, #state{polygon_function = PolygonFunction,
         reduced_set = Set} = State) ->
    case PolygonFunction(X, Y) of
        true ->
            NewSet = gb_sets:add({node, Id}, Set),
            osm_writer:write(Element),
            State#state{reduced_set = NewSet};
        _ ->
            State
    
    end;

process_message({way, Id, Nodes, Attributes, Tags}, #state{reduced_set = Set} = State) ->
    NodesInPolygon = lists:filter(fun(E) -> gb_sets:is_member({node, E}, Set) end, Nodes),
    case NodesInPolygon of
        [] ->
            State;
        List ->
            NewSet = gb_sets:add({way, Id}, Set),
            osm_writer:write({way, Id, List, Attributes, Tags}),
            State#state{reduced_set = NewSet}
    end;

process_message({relation, Id, Members, Attributes, Tags}, #state{reduced_set = Set} = State) ->
    MembersInPolygon =
        lists:filter(fun({Type, MemberId, _}) ->
                             gb_sets:is_member({Type, MemberId}, Set) end,
                     Members),
    case MembersInPolygon of
        [] ->
            State;
        List ->
            NewSet = gb_sets:add({relation, Id}, Set),
            osm_writer:write({relation, Id, List, Attributes, Tags}),
            State#state{reduced_set = NewSet}
    end;

process_message(Msg, State) ->
    io:format("Unhandled message: ~p~n", [Msg]),
    State.
