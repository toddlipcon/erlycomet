%%%-------------------------------------------------------------------
%%% File:      erlycomet_demo_rpc.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% Demo RPC custom application
%%% @end  
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
%%% @since 2007-12-08 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlycomet_demo_rpc).
-author('rsaccon@gmail.com').

%% API
-export([delayed_echo/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (string(), string()) -> string()
%% @doc
%% returns delayed the same value as it gets as input
%% @end 
%%--------------------------------------------------------------------
delayed_echo(<<"/rpc/test">>, Text, Delay) ->
    Timeout = list_to_integer(binary_to_list(Delay)) * 1000,
    receive
    after Timeout ->
    	Text
    end.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

