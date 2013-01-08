%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(erlbuild).

-behaviour(application).
-behaviour(supervisor).
-author('uaforum1@googlemail.com').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/erlbuild.hrl").
%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/0, start/2, stop/0, stop/1, restart/0]).
-export([init/1, start_link/2]).
-export([get_env/1]).

get_env(Parameter) ->
	application:get_env(?MODULE, Parameter). 
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
Cc_reloader = {cc_reloader,
 			  {cc_reloader, start_link, []},
			  permanent,
			  10000,
			  worker,
			  [cc_reloader]},
Cc_timer = {cc_timer,
			{cc_timer, start_link, []},
			permanent,
           	10000,
			worker,
            [cc_timer]},
Cc_compiler={cc_compiler,
         	 {cc_compiler, start_link, []},
             permanent,
             10000,
             worker,
             [cc_compiler]},
Cc_file_poller={cc_file_poller,
         	 {cc_file_poller, start_link, []},
              permanent,
              10000,
              worker,
              [cc_file_poller]},
{ok, {{one_for_one, 3, 10},
		   [
			Cc_reloader,
			Cc_timer,
			Cc_compiler,
			Cc_file_poller
			]}}.

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start() ->
	application:start(?MODULE).

start_link(_Type, _Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% --------------------------------------------------------------------
%% Func: stop/0
%% Returns: any
%% --------------------------------------------------------------------
stop() ->
    application:stop(?MODULE).
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.
%% --------------------------------------------------------------------
%% Func: restart/0
%% Returns: any
%% --------------------------------------------------------------------
restart() ->
    stop(),
    start().

%% ====================================================================
%% Internal functions
%% ====================================================================

