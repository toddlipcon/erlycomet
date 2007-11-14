%%%-------------------------------------------------------------------
%%% File:      erlycomet_bayeux.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Tait Larson
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% Comet Extension for MochiWeb
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
-include("../../include/erlycomet.hrl").

%% API
-export([handle/2]).

-record(state, {msgs = []}).


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
		Body ->
			Req:ok({"text/json", mochijson:encode(Body)})   
	end;
	
handle(Req, Other) ->
	?D({"not_handled: ", Other}),
	Req:not_found().


%%====================================================================
%% Internal functions
%%====================================================================

%% input: json object. output: json object result of message processing.
process_bayeux_msg(Req, {Type, Msgs}) ->
    case Type of
	    array  -> 
		    {array, [ process_msg(Req, M) || M <- Msgs ]};
	    struct -> 
		    {array, [ process_msg(Req, Msgs) ]}
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
    {struct, Resp};

process_cmd(Req, "/meta/connect", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    % ConnectionType = get_bayeux_val("connectionType", Struct),
	erlycomet_dist_server:add_connection(ClientId, self()),
    Resp = {struct, [{channel, "/meta/connect"}, 
                     {successful, true},
                     {clientId, ClientId}]},
	Req:respond({200, [], chunked}),
	loop(Req, #state{msgs = [Resp]}),
	done;

% TODO complete rewrite, this is place holder
process_cmd(_Req, "/meta/reconnect", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    case erlycomet_dist_server:connection(ClientId) of
	    undefined ->
	        Resp = [{channel, "/meta/reconnect"}, 
	                {successful, false},
	                {error, "invalid clientId"}], 
	        {struct, Resp};
	    _ ->
	        %% get events and add to response
	        Resp = [{channel, "/meta/reconnect"}, 
	                {successful, true}],             
	        _Pid = self(),
	        %%spawn(fun() -> erlycomet_dist_server:replace_connection(ClientId, self()),
	        %%               loop(Pid, ClientId) 
	        %%      end),
	        %%[{header, {cache_control, "no-cache"}},
	        %% {streamcontent_with_timeout, "content_type(Props)", Response, infinity}] %% needs to be adapted
			{struct, Resp} %% needs to be adapted
	end.
	
	
generate_id() ->
    <<Num:128>> = crypto:rand_bytes(16),
    [HexStr] = io_lib:fwrite("~.16B",[Num]),
	case erlycomet_dist_server:connection(HexStr) of
		undefined ->
    		HexStr;
		_ ->
			generate_id()
	end.


loop(Req, State) ->
    receive
        stop ->  
            erlycomet_dist_server:remove_connection(self());
        {flush, Response} -> 
			Msgs = lists:reverse([Response | State#state.msgs]),
            Req:respond(mochijson:encode({array, Msgs})),
			loop(Req, State#state{msgs=[]})
    end.