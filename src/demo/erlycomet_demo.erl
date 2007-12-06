%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        Helper module for easy application start, stop, reloading , etc.
%%% @reference  See <a href="http://erlycomet.googlecode.com" target="_top">http://erlycomet.googlecode.com</a> for more information
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
-module(erlycomet_demo
).
-author('rsaccon@gmail.com').

%% API
-export ([start/0, stop/0, reload/0]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc start erlycomet demo application
%% @end 
%%--------------------------------------------------------------------
start() -> 
	io:format("Starting ErlyComet Demo...~n"),
	ensure_started(crypto),
	application:start(erlycomet).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc stop erlycomet demo application
%% @end 
%%--------------------------------------------------------------------
stop() ->
	io:format("Stopping ErlyComet Demo...~n"),
	application:stop(erlycomet).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc reload erlycomet modules
%% @end 
%%--------------------------------------------------------------------
reload() ->
	io:format("Compiling and reloading ErlyComet Modules ...~n"),
	make:all([load]).
	

%%====================================================================
%% Internal functions
%%====================================================================	
ensure_started(App) ->
    case application:start(App) of
		ok ->
	    	ok;
		{error, {already_started, App}} ->
	    	ok
    end.
