%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Roberto Saccon <telarson@gmail.com>
%%% @copyright  2007 Roberto Saccon, Tait Larson
%%% @doc        gloabl server
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Tait Larson
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
-author('telarson@gmail.com').

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


-record(state, {}).

-record(connection, {client_id, pid}).

-record(channel, {channel, client_ids}). %rename subscriptions

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    Result = gen_server_cluster:start(?MODULE, ?MODULE, [], []),
    Node = node(),
    %% TODO: wait for other nodes having started ???????
    case catch gen_server_cluster:get_all_server_nodes(?MODULE) of
	    {Node, LocalNodes} ->
	        up_master(LocalNodes);
        _ ->
	        up_slave(Node)
    end,
    Result.

stop() -> 
    mnesia:stop(), %TODO: NEED To stop mnesia on each node.
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


%%-------------------------------------------------------------------------
%% @spec 
%% @doc 
%% @end
%%-------------------------------------------------------------------------
add_connection(ClientId, Pid) -> 
    Row = #connection{client_id=ClientId, pid=Pid},
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, 1111]),
    F = fun() ->
		mnesia:write(Row)
	end,
	io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, 2222]),
    {atomic, Result} = mnesia:transaction(F),
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, 3333]),
    Result.
 
  
%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------    
connections() -> 
    Recs = do(qlc:q([X || X <-mnesia:table(connection)])),
    [{Y,Z} || {connection, Y, Z} <- Recs].
 
 
%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------    
connection(ClientId) ->
    F = fun() ->
        mnesia:read({connection, ClientId})
    end,
    {atomic, Row} = mnesia:transaction(F),
    case Row of
        [] ->
            undefined;
        [{connection, ClientId, Pid}] ->
            Pid
    end.


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------	
remove_connection(ClientId) ->
    Oid = {connection, ClientId},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
replace_connection(ClientId, Pid) ->
    add_connection(ClientId, Pid).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
subscribe(ClientId, Channel) ->
    F = fun() ->
        ClientIdList = case mnesia:read({channel, Channel}) of
            [] -> 
                [ClientId];
            [{channel, Channel, []} ] ->
                [ClientId];
            [{channel, Channel, [Ids]} ] ->
                [ClientId | Ids]
        end,
        mnesia:write(#channel{channel=Channel, client_ids=ClientIdList})
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
unsubscribe(ClientId, Channel) ->
    F = fun() ->
		case mnesia:read({channel, Channel}) of
		    [] ->
			{error, channel_not_found};
		    [{channel, Channel, Ids}] ->
			mnesia:write({channel, Channel, lists:delete(ClientId,  Ids)})
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
channels() ->
    Recs = do(qlc:q([X || X <-mnesia:table(channel)])),
    [{Y,Z} || {channel, Y, Z} <- Recs].



%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
deliver_to_connection(ClientId, Message) ->
    F = fun() -> 
		mnesia:read({connection, ClientId})
	end,
    case mnesia:transaction(F) of 
		{atomic, []} ->
		    {error, connection_not_found};
		{atomic, [{connection, ClientId, Pid}] } -> 
		    Pid ! Message,
		    ok
	end.
	

%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
deliver_to_channel(Channel, Message) ->
    F = fun() -> 
        mnesia:read({channel, Channel})
    end,
    case mnesia:transaction(F) of 
        {atomic, []} ->
            {error, channel_not_found};
        {atomic, [{channel, Channel, []}] } -> 
            ok;
        {atomic, [{channel, Channel, [Ids]}] } -> 
            [connection(ClientId) ! Message || ClientId <- Ids],
            ok
     end.


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

handle_call(_Request, _From, State) ->
    Reply = ok,
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

% do this before up_slave
up_master(Slaves) ->
    _FirstRun = case mnesia:create_schema([node()]) of
        {error, {_Node, {already_exists, _Node}}} -> 
            false;
        ok -> 
            true
    end,    
    mnesia:start(),
    mnesia:create_table(connection, [{attributes, record_info(fields, connection)}, {ram_copies, node()}]),
    mnesia:create_table(channel, [{attributes, record_info(fields, channel)}, {ram_copies, node()}]),
    F = fun(N) ->  %% rsaccon IDEA: this should be reqested by non-global node via rpc or message passing
		mnesia:add_table_copy(schema, N, ram_copies)
	end,
    lists:foreach(F, Slaves).

% do this last    
up_slave(Master) ->
    %% rsaccon IDEA: reqest from gobal node to execute mnesia:add_table_copy/3
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [Master]).

do(QLC) ->
    F = fun() ->
		 qlc:e(QLC) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

