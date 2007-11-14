%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        gloabl server
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(erlycomet_dist_server).
-author('rsaccon@gmail.com').
-include("../../include/erlycomet.hrl").

-behaviour(gen_server).


%% API
-export([start/0, 
         stop/0,
		 is_global/0,
         add_connection/2,
         connections/0,
         connection/1,
         remove_connection/1,
         replace_connection/2,
         subscribe/2,
         unsubscribe/2,
         channels/0,
         deliver_to_connection/2,
         deliver_to_channel/2]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

% rsaccon: TODO: disrubuted mnesia RAM tables instead of ets
-record(state, {connections = ets:new(connections_table, []),
                channels = ets:new(channels_table, [])}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server_cluster:start(?MODULE, ?MODULE, [], []).

stop() -> 
    gen_server:stop({global, ?MODULE}).

%%-------------------------------------------------------------------------
%% @spec () -> bool()
%% @doc global or local server
%% @end
%%-------------------------------------------------------------------------
is_global() ->
    LocalNode = node(),
    case catch gen_server_cluster:get_all_server_nodes(?MODULE) of
	    {LocalNode, _} ->
	        true;
        _ ->
	        false
    end.

%%
% rsaccon: TODO: implement all the functiones below wwith dist. mnesia RAM tables instead of ets
%
add_connection(ClientId, Pid) ->
    gen_server:call({global,?MODULE}, {add_connection, ClientId, Pid}). 

connections() ->
    gen_server:call({global,?MODULE}, {connections}). 
    
connection(ClientId) ->
    gen_server:call({global,?MODULE}, {connection, ClientId}). 

remove_connection(Pid) when is_pid(Pid)->
    ?D(not_implemented_yet);
	
remove_connection(ClientId) ->
    gen_server:call({global,?MODULE}, {remove_connection, ClientId}).

replace_connection(ClientId, Pid) ->
    gen_server:call({global,?MODULE}, {replace_connection, ClientId, Pid}).

subscribe(ClientId, Channel) ->
    gen_server:call({global,?MODULE}, {subscribe, ClientId, Channel}).

unsubscribe(ClientId, Channel) ->
    gen_server:call({global,?MODULE}, {unsubscribe, ClientId, Channel}).

channels() ->
    gen_server:call({global,?MODULE}, {channels}).

deliver_to_connection(ClientId, Message) ->
    gen_server:call({global,?MODULE}, {deliver_to_connection, ClientId, Message}).

deliver_to_channel(Channel, Message) ->
    gen_server:call({global,?MODULE}, {deliver_to_channel, Channel, Message}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------


handle_call({add_connection, ClientId, Pid}, _From, State) ->
    ets:insert(State#state.connections, {ClientId, Pid}),
    Reply = ok,
    {reply, Reply, State};

handle_call({connections}, _From, State) ->
    Reply = ets:tab2list(State#state.connections),
    {reply, Reply, State};

handle_call({connection, ClientId}, _From, State) ->
    Reply = case ets:lookup(State#state.connections, ClientId) of
                [] ->
                    undefined;
                [Pid] ->
                    Pid
            end,
    {reply, Reply, State};

handle_call({remove_connection, ClientId}, _From, State) ->
    ets:delete(State#state.connections, ClientId),
    Reply = ok,
    {reply, Reply, State};

handle_call({replace_connection, ClientId, Pid}, _From, State) ->
    ets:delete(State#state.connections, ClientId),
    ets:insert(State#state.connections, {ClientId, Pid}),
    Reply = ok,
    {reply, Reply, State};

handle_call({subscribe, ClientId, Channel}, _From, State) ->
    Reply = case ets:lookup(State#state.channels, Channel) of
                [] ->
                    ets:insert(State#state.channels, {Channel, [ClientId]}),
                    ok;
                [ClientIdList] ->
                    ets:insert(State#state.channels, {Channel, [ClientId | ClientIdList]}),
                    ok
            end,
    {reply, Reply, State};


handle_call({unsubscribe, ClientId, Channel}, _From, State) ->
    Reply = case ets:lookup(State#state.channels, Channel) of
                [] ->
                    {error, channel_not_found};
                [ClientIdList] ->
                    ets:insert(State#state.channels, {Channel, lists:delete(ClientId, ClientIdList)}),
                    ok
            end,
    {reply, Reply, State};

handle_call({channels}, _From, State) ->
    Reply = ets:tab2list(State#state.channels),
    {reply, Reply, State};

handle_call({deliver_to_connection, ClientId, Message}, _From, State) ->
    Reply = case ets:lookup(State#state.connections, ClientId) of
                [] ->
                    {error, connection_not_found};
                [Pid] ->
                    Pid ! Message,
                    ok
            end,
    {reply, Reply, State};

handle_call({deliver_to_channel, Channel, Message}, _From, State) ->
    Reply = case ets:lookup(State#state.channels, Channel) of
                [] ->
                    {error, channel_not_found};
                [ClientIdList] ->
                    [connection(ClientId) ! Message || ClientId <- ClientIdList],
                    ok
            end,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
