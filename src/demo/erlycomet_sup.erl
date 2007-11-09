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
-module(erlycomet_sup).
-author('rsaccon@gmail.com').
-include("../../include/erlycomet.hrl").

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, upgrade/0]).

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).


%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.
	
	
%% ====================================================================
%% Supervisor functions
%% ====================================================================

init([]) ->	
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxTimeBetweenRestarts = 10,
    SupFlags  = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},
    ErlyComet = {erlycomet_cluster,
                 {erlycomet_cluster, start, []},
                 permanent,
                 1000,
                 worker,
                 [erlycomet_cluster]},
    MochiWeb = {erlycomet_http, 
				{erlycomet_http, start, []},
                permanent,
                1000,
                worker,
                [erlycomet_http]},
    {ok,{SupFlags, [ErlyComet, MochiWeb]}}.
	
