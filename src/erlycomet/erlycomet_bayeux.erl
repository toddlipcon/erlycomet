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
				msgs = [],
				timeout = 20000}).  % is low just for testing


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

handle(Req, [{"message", Msg}]) ->
	case process_bayeux_msg(Req, mochijson:decode(Msg)) of
		done ->
			ok;
		{array,[done]} ->
			ok;
		Body ->
			io:format("TRACE ~p:~p body: ~p~n",[?MODULE, ?LINE, Body]),
			Req:ok({"text/json", mochijson:encode(Body)})   
	end,
	io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, shit]);
	
handle(Req, Other) ->
	io:format("TRACE ~p:~p not handled: ~p~n",[?MODULE, ?LINE, Other]),
	Req:not_found().


%%====================================================================
%% Internal functions
%%====================================================================

%% input: json object. output: json object result of message processing.
process_bayeux_msg(Req, {Type, Content}=Struct) ->
    case Type of
	    array  -> 
		    {array, [ process_msg(Req, M) || M <- Content ]};
	    struct -> 
		    %{array, [ process_msg(Req, Msgs) ]}
			process_msg(Req, Struct)
    end.


process_msg(Req, Struct) ->
	 process_cmd(Req, get_bayeux_val("channel", Struct), Struct).


get_bayeux_val(Key, {struct, Pairs}) when is_list(Pairs) ->
	case [ V || {K, V} <- Pairs, K =:= Key] of
		[] ->
			undefined;
		[ V | _Rest ] ->
    		V
    end;
get_bayeux_val(_, _) ->
	undefined.


process_cmd(_Req, "/meta/handshake", _Struct) ->	
	% Advice = {struct, [{reconnect, "retry"},
    %                   {interval, 5000}]},
    Resp = [{channel, "/meta/handshake"}, 
            {version, 1.0},
            {supportedConnectionTypes, {array, ["long-polling",
												"callback-polling"]}},
            {clientId, generate_id()},
            {successful, true}],
    % Resp2 = [{advice, Advice} | Resp],
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, eeee]),
    {struct, Resp};

process_cmd(Req, "/meta/connect", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    % ConnectionType = get_bayeux_val("connectionType", Struct),
	R = erlycomet_dist_server:add_connection(ClientId, self()),
	
	%% rsaccon: TODO; case when erlycomet_dist_server:add_connection/2 fails
	io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, R]),
	
    Msg = {struct, [{"channel", "/meta/connect"}, 
                     {"successful", true},
                     {"clientId", ClientId}]},
	Resp = Req:respond({200, [], chunked}),
	loop(Resp, #state{id = ClientId, msgs = [Msg]});
	
process_cmd(_Req, "/meta/disconnect", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    erlycomet_dist_server:remove_connection(ClientId),
	%% rsaccon: TODO; case when erlycomet_dist_server:add_connection/2 fails
	Resp = [{channel, "/meta/disconnect"}, 
            {successful, true},
            {clientId, ClientId}],
	{struct, Resp};

process_cmd(_Req, "/meta/subcribe", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
	Subscription = get_bayeux_val("subscription", Struct),
    erlycomet_dist_server:subscribe(ClientId, Subscription),
	%% rsaccon: TODO; case when erlycomet_dist_server:subscribe/2 fails
	Resp = [{channel, "/meta/subcribe"}, 
            {successful, true},
            {clientId, ClientId},
            {subscription, Subscription}],
	{struct, Resp};
	
process_cmd(_Req, "/meta/unsubcribe", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
	Subscription = get_bayeux_val("subscription", Struct),
    erlycomet_dist_server:unsubscribe(ClientId, Subscription),
	%% rsaccon: TODO; case when erlycomet_dist_server:unsubscribe/2 fails
	Resp = [{channel, "/meta/unsubcribe"}, 
            {successful, true},
            {clientId, ClientId},
            {subscription, Subscription}],
	{struct, Resp};
			
process_cmd(_Req, Channel, Struct) ->	
    _ClientId = get_bayeux_val("clientId", Struct),
    Data = get_bayeux_val("data", Struct),
    erlycomet_dist_server:deliver_to_channel(Channel, Data),
	%% rsaccon: TODO; case when erlycomet_dist_server:deliver_to_channel/2 fails
	Resp = [{channel, Channel}, 
            {successful, true}],
	{struct, Resp}.
		
			
generate_id() ->
    <<Num:128>> = crypto:rand_bytes(16),
    [HexStr] = io_lib:fwrite("~.16B",[Num]),
    case erlycomet_dist_server:connection(HexStr) of
        undefined ->
            HexStr;
     _ ->
        generate_id()
    end.


loop(Resp, State) ->
    receive
        stop ->  
            disconnect(Resp, State);
        {flush, Msg} -> 
			Msgs = lists:reverse([Msg | State#state.msgs]),
            Resp:write_chunk(mochijson:encode({array, Msgs})),
			loop(Resp, State#state{msgs=[]})
	after State#state.timeout ->
		disconnect(Resp, State)
    end.


disconnect(Resp, State) ->
	erlycomet_dist_server:remove_connection(State#state.id),
	Msg = {struct, [{"channel", "/meta/disconnect"}, 
                    {"successful", true},
                    {"clientId", State#state.id}]},
	Resp:write_chunk(mochijson:encode(Msg)),
	Resp:write_chunk([]),
	done.