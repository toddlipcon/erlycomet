%%%-------------------------------------------------------------------
%%% File:      erlycomet_bayeux.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Tait Larson
%%% @copyright 2007 Roberto Saccon, Tait Larson
%%% @doc Comet Extension for MochiWeb
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end  
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
%%% @since 2007-11-11 by Roberto Saccon, Tait Larson
%%%-------------------------------------------------------------------
-module(erlycomet_bayeux).
-author('telarson@gmail.com').
-author('rsaccon@gmail.com').


%% API
-export([handle/2]).

-record(state, {id = undefined,
				events = [],
				timeout = 1200000,      % 20 min, just for testing
				callback = undefined}).  


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec
%% @doc handle POST message
%% @end 
%%--------------------------------------------------------------------
handle(Req, 'POST') ->
	handle(Req, Req:parse_post());
	
handle(Req, 'GET') ->
	handle(Req, Req:parse_qs());	

handle(Req, [{"message", Msg}, {"jsonp", Callback} | _]) ->
	case process_bayeux_msg(Req, mochijson:decode(Msg), Callback) of
		done ->
			ok;
		{array,[done]} ->
			ok;
		Body ->
		    Resp = callback_wrapper(mochijson:encode(Body), Callback),
			Req:ok({"text/javascript", Resp})   
	end;
    	
handle(Req, [{"message", Msg} | _]) ->
	case process_bayeux_msg(Req, mochijson:decode(Msg), undefined) of
		done ->
			ok;
		{array,[done]} ->
			ok;
		Body ->
			Req:ok({"text/json", mochijson:encode(Body)})   
	end;
    	
handle(Req, _) ->
	Req:not_found().


%%====================================================================
%% Internal functions
%%====================================================================

%% input: json object. output: json object result of message processing.
process_bayeux_msg(Req, {Type, Content}=Struct, Callback) ->
    case Type of
	    array  -> 
		    {array, [ process_msg(Req, M, Callback) || M <- Content ]};
	    struct -> 
		    %{array, [ process_msg(Req, Msgs) ]}  ???????????
			process_msg(Req, Struct, Callback)
    end.


process_msg(Req, Struct, Callback) ->
	 process_cmd(Req, get_bayeux_val("channel", Struct), Struct, Callback).


get_bayeux_val(Key, {struct, Pairs}) when is_list(Pairs) ->
	case [ V || {K, V} <- Pairs, K =:= Key] of
		[] ->
			undefined;
		[ V | _Rest ] ->
    		V
    end;
get_bayeux_val(_, _) ->
	undefined.


process_cmd(_Req, "/meta/handshake", _Struct, _) ->	
	% Advice = {struct, [{reconnect, "retry"},
    %                   {interval, 5000}]},
    Resp = [{channel, "/meta/handshake"}, 
            {version, 1.0},
            {supportedConnectionTypes, {array, ["long-polling",
												"callback-polling"]}},
            {clientId, generate_id()},
            {successful, true}],
    % Resp2 = [{advice, Advice} | Resp],
    {struct, Resp};

process_cmd(Req, "/meta/connect", Struct, Callback) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    _ConnectionType = get_bayeux_val("connectionType", Struct),
	L = [{"channel", "/meta/connect"}, 
         {"clientId", ClientId}],    
    case erlycomet_dist_server:replace_connection(ClientId, self()) of
        {ok, new} ->
           {struct, [{"successful", true} | L]};
	    {ok, replaced} ->	
            Msg = {struct, [{"successful", true} | L]},
	        Resp = Req:respond({200, [], chunked}),
	        loop(Resp, #state{id = ClientId, 
	                          events = [Msg],
	                          callback = Callback});
	    _ ->
        	{struct, [{"successful", false} | L]}
    end;
	
process_cmd(_Req, "/meta/disconnect", Struct, _) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    %% rsaccon: TODO; what do we do if there is no valid ClientId ?
	L = [{"channel", "/meta/disconnect"}, 
         {"clientId", ClientId}],
    case erlycomet_dist_server:remove_connection(ClientId) of
	    ok -> {struct, [{"successful", true}  | L]};
  	    _ ->  {struct, [{"successful", false}  | L]}
	end;    

process_cmd(_Req, "/meta/subscribe", Struct, _) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    %% rsaccon: TODO; what do we do if there is no valid ClientId ?
	Subscription = get_bayeux_val("subscription", Struct),
	L = [{"channel", "/meta/subscribe"}, 
         {"clientId", ClientId},
         {"subscription", Subscription}],
    case erlycomet_dist_server:subscribe(ClientId, Subscription) of
	    ok -> {struct, [{"successful", true}  | L]};
  	    _ ->  {struct, [{"successful", false}  | L]}
	end;	
	
process_cmd(_Req, "/meta/unsubcribe", Struct, _) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    %% rsaccon: TODO; what do we do if there is no valid ClientId ?
	Subscription = get_bayeux_val("subscription", Struct),
	L = [{"channel", "/meta/unsubcribe"}, 
         {"clientId", ClientId},
         {"subscription", Subscription}],          
    case erlycomet_dist_server:unsubscribe(ClientId, Subscription) of
	    ok -> {struct, [{"successful", true}  | L]};
  	    _ ->  {struct, [{"successful", false}  | L]}
	end;
			
process_cmd(_Req, Channel, Struct, _) ->	
    Data = get_bayeux_val("data", Struct),
    L = case get_bayeux_val("clientId", Struct) of
        undefined ->
            %% rsaccon: TODO; do we actually allow that ?
            [{"channel", Channel}];
        ClientId ->
            [{"channel", Channel}, 
             {"clientId", ClientId}]
    end,    
    case erlycomet_dist_server:deliver_to_channel(Channel, Data) of
   	    ok -> {struct, [{"successful", true}  | L]};
   	    _ ->  {struct, [{"successful", false}  | L]}
   	end.


callback_wrapper(Data, undefined) ->
    Data;		
callback_wrapper(Data, Callback) ->
    lists:concat([Callback, "(", Data, ");"]).
    
    			
generate_id() ->
    <<Num:128>> = crypto:rand_bytes(16),
    [HexStr] = io_lib:fwrite("~.16B",[Num]),
    case erlycomet_dist_server:connection(HexStr) of
        undefined ->
            HexStr;
     _ ->
        generate_id()
    end.


loop(Resp, #state{callback = Callback} = State) ->
    receive
        stop ->  
            disconnect(Resp, State);
        {event, Event} -> 
    		Events = lists:reverse([Event | State#state.events]),
            Resp:write_chunk(mochijson:encode({array, Events})),
    		loop(Resp, State#state{events=[]});           
        flush -> 
			Events = lists:reverse([State#state.events]),
			Chunk = callback_wrapper(mochijson:encode({array, Events}), Callback),
            Resp:write_chunk(Chunk),
			loop(Resp, State#state{events=[]})
	after State#state.timeout ->
		disconnect(Resp, State)
    end.


disconnect(Resp, #state{callback = Callback} = State) ->
	erlycomet_dist_server:remove_connection(State#state.id),
	Msg = {struct, [{"channel", "/meta/disconnect"}, 
                    {"successful", true},
                    {"clientId", State#state.id}]},
    Chunk = callback_wrapper(mochijson:encode(Msg), Callback),
    Resp:write_chunk(Chunk),
	Resp:write_chunk([]),
	done.