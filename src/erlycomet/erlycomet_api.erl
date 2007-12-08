%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Roberto Saccon <telarson@gmail.com>
%%% @copyright  2007 Roberto Saccon, Tait Larson
%%% @doc        gloabl server and mnesia broker
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
-module(erlycomet_api).
-author('rsaccon@gmail.com').
-author('telarson@gmail.com').
-include("erlycomet.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([add_connection/2,
         replace_connection/2,
         connections/0,
         connection/1,
         remove_connection/1,
         subscribe/2,
         unsubscribe/2,
         channels/0,
         deliver_to_connection/3,
         deliver_to_channel/2]).

 

%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------------
%% @spec (string(), pid()) -> ok | error 
%% @doc
%% adds a connection
%% @end
%%-------------------------------------------------------------------------
add_connection(ClientId, Pid) -> 
    Row = #connection{client_id=ClientId, pid=Pid},
    F = fun() ->
		mnesia:write(Row)
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.
 
 
%%-------------------------------------------------------------------------
%% @spec (string(), pid()) -> {ok, new} | {ok, replaced} | error 
%% @doc
%% replaces a connection
%% @end
%%-------------------------------------------------------------------------
replace_connection(ClientId, Pid) -> 
    E = #connection{client_id=ClientId, pid=Pid},
    F1 = fun() ->
        mnesia:read({connection, ClientId})
    end,
    {Status, F2} = case mnesia:transaction(F1) of
        {atomic, Row} ->
            case Row of
                [] ->
                    {new, fun() -> mnesia:write(E) end};            
                [_] ->
                    {replaced, fun() -> mnesia:write(E) end}
            end;
        _ ->
            {new, fun() -> mnesia:write(E) end}
    end,
    case mnesia:transaction(F2) of
        {atomic, ok} -> {ok, Status};
        _ -> error
    end.
       
          
%%--------------------------------------------------------------------
%% @spec () -> list()
%% @doc
%% returns list of connections
%% @end 
%%--------------------------------------------------------------------    
connections() -> 
    Recs = do(qlc:q([X || X <-mnesia:table(connection)])),
    [{Y,Z} || {connection, Y, Z} <- Recs].
 
 
%%--------------------------------------------------------------------
%% @spec (string()) -> pid()
%% @doc 
%% returns the PID of a connection if it exists
%% @end 
%%--------------------------------------------------------------------    
connection(ClientId) ->
    F = fun() ->
        mnesia:read({connection, ClientId})
    end,
    case mnesia:transaction(F) of
        {atomic, Row} ->
            case Row of
                [] ->
                    undefined;
                [{connection, ClientId, Pid}] ->
                    Pid
            end;
        _ ->
            undefined
    end.
    

%%--------------------------------------------------------------------
%% @spec (string()) -> ok | error  
%% @doc
%% removes a connection
%% @end 
%%--------------------------------------------------------------------	
remove_connection(ClientId) ->
    Oid = {connection, ClientId},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.	


%%--------------------------------------------------------------------
%% @spec (string(), string()) -> ok | error 
%% @doc
%% subscribes a client to a channel
%% @end 
%%--------------------------------------------------------------------
subscribe(ClientId, Channel) ->
    F = fun() ->
        ClientIdList = case mnesia:read({channel, Channel}) of
            [] -> 
                [ClientId];
            [{channel, Channel, []} ] ->
                [ClientId];
            [{channel, Channel, Ids} ] ->
                [ClientId | Ids]
        end,
        mnesia:write(#channel{channel=Channel, client_ids=ClientIdList})
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.


%%--------------------------------------------------------------------
%% @spec (string(), string()) -> ok | error  
%% @doc
%% unsubscribes a client from a channel
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
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        O ->
            io:format("TRACE ~p:~p remove ~p~n",[?MODULE, ?LINE, O]),
            error
    end.


%%--------------------------------------------------------------------
%% @spec () -> list()
%% @doc
%% returns a list of channels
%% @end 
%%--------------------------------------------------------------------
channels() ->
    Recs = do(qlc:q([X || X <-mnesia:table(channel)])),
    [{Y,Z} || {channel, Y, Z} <- Recs].



%%--------------------------------------------------------------------
%% @spec (string(), string(), tuple()) -> ok | {error, connection_not_found} 
%% @doc
%% delivers data to one connection
%% @end 
%%--------------------------------------------------------------------
deliver_to_connection(ClientId, Channel, Data) ->
    Event = {struct, [{"channel", Channel}, 
                      {"data", Data}]},
    F = fun() -> 
		mnesia:read({connection, ClientId})
	end,
    case mnesia:transaction(F) of 
		{atomic, []} ->
		    {error, connection_not_found};
		{atomic, [{connection, ClientId, Pid}] } -> 
		    Pid ! {event, Event},
		    ok
	end.
	

%%--------------------------------------------------------------------
%% @spec  (string(), tuple()) -> ok | {error, channel_not_found} 
%% @doc
%% delivers data to all connections of a channel
%% @end 
%%--------------------------------------------------------------------
deliver_to_channel(Channel, Data) ->
    Event = {struct, [{"channel", Channel}, 
                      {"data", Data}]},                    
    F = fun() -> 
        mnesia:read({channel, Channel})
    end,
    case mnesia:transaction(F) of 
        {atomic, []} ->
            {error, channel_not_found};
        {atomic, [{channel, Channel, []}] } -> 
            ok;
        {atomic, [{channel, Channel, Ids}] } ->
            [send_event(connection(ClientId), Event) || ClientId <- Ids],
            ok;
        _ ->
            {error, channel_not_found}
     end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send_event(Pid, Event) when is_pid(Pid)->
    Pid ! {flush, Event};
send_event(_, _) ->
    ok.


do(QLC) ->
    F = fun() ->
		 qlc:e(QLC) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

