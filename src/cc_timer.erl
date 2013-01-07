%%% -------------------------------------------------------------------
%%% Author  : uaforum1@googlemail.com'
%%% Description : This modules implements a kind of timer which 
%%% sends message to the clients which are described in the erlbuild.app
%%% file.
%%% The clients has to implement the function time_triggered/1
%%% Created : 29.10.2010 
%%% -------------------------------------------------------------------
-module(cc_timer).

-behaviour(gen_server).
-author('uaforum1@googlemail.com').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0, get_timer/0]).

-define(DEBUG(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}

%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}, 0}.
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
	%%?DEBUG("...Timeout"),
	handle_info({next_run}, State);
handle_info({next_run}, State) ->
	%%?DEBUG("...Next Run"),
	List_of_clients = get_timer_clients(),
	send_msg_to_clients(List_of_clients),
	start_timer(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_timer() ->
	case erlbuild:get_env(timer_interval) of
		{ok, Value} -> Value;
		undefined -> error_logger:warning_msg("There is noch timer_interval configured. Please, have a look at your erlbuild.app"),
					10000
	end.
get_timer_clients() ->
	case erlbuild:get_env(timer_clients) of
		{ok, Value} -> Value;
		undefined -> error_logger:warning_msg("There are no timer_clients configured. Please, have a look at your erlbuild.app"),
					 []
	end.
start_timer() ->
	erlang:send_after(get_timer(), self(), {next_run}).
send_msg_to_clients(List_of_clients) ->
	%%?DEBUG("... send_msg_to_clients"),
	[send_msg_to_client(Client) || Client <- List_of_clients].
send_msg_to_client(Client) ->
	%%?DEBUG("... send_msg_to_client"),
	Client:time_triggered([]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_timer_with_configured_value_test() ->
	application:load(erlbuild),
	?assertEqual(2000, get_timer()).	
get_timer_clients_with_configured_value_test() ->	
	application:load(erlbuild),
	?assertEqual([cc_file_poller], get_timer_clients()).	
-endif.
