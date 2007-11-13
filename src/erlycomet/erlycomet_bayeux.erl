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
-export([handle/1]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec
%% @doc handle POST message
%% @end 
%%--------------------------------------------------------------------
handle([{"message", Msg}]) ->
	case process_bayeux_msg(mochijson:decode(Msg)) of
		{array,[chunked]} ->  % rsaccon: TODO better solution for initaliting chunked stuff
			{continue, {200, [], chunked}};
		Body ->
			HResp = mochiweb_headers:make([]),
		    HResp1 = mochiweb_headers:enter("Content-Type", "text/json" ,HResp),
			{done, {200, HResp1, mochijson:encode(Body)}}  % Req:ok({"text/json", mochijson:encode(Body)});  
	end;
	
handle(Other) ->
	?D({"not_handled: ", Other}),
	error.
	

%%====================================================================
%% Internal functions
%%====================================================================

%% input: json object. output: json object result of message processing.
process_bayeux_msg({Type, Msgs}) ->
    case Type of
	    array  -> 
		    {array, [ process_msg(M) || M <- Msgs ]};
	    struct -> 
		    {array, [ process_msg(Msgs) ]}
    end.

process_msg(Struct) ->
	 process_cmd(get_bayeux_val("channel", Struct), Struct).

get_bayeux_val(Key, {struct, Pairs}) when is_list(Pairs) ->
	case [ V || {K, V} <- Pairs, K =:= Key] of
		[] ->
			undefined;
		[ V | _Rest ] ->
    		V
    end;
get_bayeux_val(_, _) ->
	undefined.

process_cmd("/meta/handshake", _Struct) ->	
	% Advice = {struct, [{reconnect, "retry"},
    %                   {interval, 5000}]},
    Resp = [{channel, "/meta/handshake"}, 
            {version, 1.0},
            {supportedConnectionTypes, {array, ["long-polling",
												"callback-polling"]}},
            {clientId, generate_id()},
            {successful, true}],
    % L2 = [{advice, Advice} | L],
    {struct, Resp};

process_cmd("/meta/connect", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    ConnectionType = get_bayeux_val("connectionType", Struct),
	?D(ConnectionType),
	erlycomet_dist_server:add_connection(ClientId, self()),
    Resp = [{channel, "/meta/connect"}, 
            {successful, true},
            {clientId, ClientId}],
    % put this on a buffer: {struct, Resp}};
	chunked;

% TODO complete rewrite, this is place holder
process_cmd("/meta/reconnect", Struct) ->	
    ClientId = get_bayeux_val("clientId", Struct),
    case erlycomet_dist_server:connection(ClientId) of
	    {error, _} ->
	        Resp = [{channel, "/meta/reconnect"}, 
	                {successful, false},
	                {error, "invalid clientId"}], 
	        {struct, Resp};
	    _ ->
	        %% get events and add to response
	        Resp = [{channel, "/meta/reconnect"}, 
	                {successful, true}],             
	        Pid = self(),
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
	% TODO: check whether it is not taken already, if yes, return generate_id().
    HexStr.