%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uangermann@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(cc_controller).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlbuild.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).

-export([process_files/1]).
-record(state, {pwd}).

%% ====================================================================
%% External functions
%% ====================================================================
process_files(List_of_files) ->
	gen_server:cast(?MODULE, {process_files, List_of_files}).
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
	{ok, Pwd} = file:get_cwd(),
    {ok, #state{pwd=Pwd}}.

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
handle_cast({process_files, List_of_files}, State) ->
	error_logger:info_msg("processing files : ~n ~p ~n", [List_of_files]),
	{ok, Project_dir} = ?PROPERTY(project_dir),
	change_working_dir(Project_dir),
	make(List_of_files),
	change_working_dir(State#state.pwd),
	code_reloader:reload_modules(),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
%% --------------------------------------------------------------------
%% Func: make/0
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
make(Files) ->
	error_logger:info_msg("now compile the files~n"),
	{ok, Compiler_options} = ?PROPERTY(compiler_options),
	case make:files(Files, Compiler_options) of
		error -> error_logger:info_msg("ERROR. Now we have to send a mail, but it is not implemented yet! ~n");
		up_to_date -> code_reloader:reload_modules()
	end.
%% --------------------------------------------------------------------
%%% 
%% --------------------------------------------------------------------
change_working_dir(Project_dir) ->
	file:set_cwd(Project_dir).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------