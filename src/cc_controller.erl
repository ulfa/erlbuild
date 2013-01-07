%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com'
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(cc_controller).

-behaviour(gen_server).
-author('uaforum1@googlemail.com').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).

-export([process_files/2]).
-record(state, {}).
-define(DEBUG(Var), io:format("DEBUG: ~p:~p - ~p~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
%% ====================================================================
%% External functions
%% ====================================================================
process_files(src, Files) ->
	gen_server:cast(?MODULE, {process_files, src, Files});
process_files(dtl, Files) ->
	gen_server:cast(?MODULE, {process_files, dtl, Files}).
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
    {ok, #state{}}.
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
handle_cast({process_files, src, List_of_files}, State) ->
	compile(src, List_of_files),
	code_reloader:reload_modules(),
    {noreply, State};
handle_cast({process_files, dtl, List_of_files}, State) ->
	compile(dtl, List_of_files),
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
get_compiler_options() ->
	case erlbuild:get_env(compiler_options) of
		{ok, Value} -> Value;
		undefined -> []
	end.
get_erlydtl_options() ->
	case erlbuild:get_env(erlydtl_options) of
		{ok, Value} -> Value;
		undefined -> []
	end.
%% --------------------------------------------------------------------
%% Func: make/0
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
compile(src, Files) ->
	?DEBUG(Files),	
	Options = get_compiler_options(),
	[compile(src, F, Options) || F <- Files];
compile(dtl, Files) ->
	?DEBUG(Files),	
	Options = get_erlydtl_options(),
	?DEBUG(Options),	
	[compile(dtl, F, Options) || F <- Files].
	
compile(src, [], _Options) ->
	?DEBUG("nothing to do");	
compile(src, File, Options) ->
	?DEBUG(File),
	case compile:file(File, [return|Options]) of
		{ok, _Module, Warnings} ->
			%% Compiling didn't change the beam code. Don't reload...
			print_results([], File, [], Warnings),
			code_reloader:reload_modules(),
			{ok, [], Warnings};
		{error, Errors, Warnings} ->
			%% Compiling failed. Print the warnings and errors...
			print_results([], File, Errors, Warnings),
			{ok, Errors, Warnings}
	end;
compile(dtl, [], _Options) ->
	?DEBUG("nothing to do");		
compile(dtl, File, Options) ->
	erlydtl:compile("./templates", filename:basename(File), Options).	
	
print_results(_Module, _SrcFile, [], []) ->
    %% Do not print message on successful compilation;
    %% We already get a notification when the beam is reloaded.
    ok;

print_results(_Module, SrcFile, [], Warnings) ->
    Msg = [
        format_errors(SrcFile, [], Warnings),
        io_lib:format("~s:0: Recompiled with ~p warnings~n", [SrcFile, length(Warnings)])
    ],
    error_logger:info_msg(lists:flatten(Msg));

print_results(_Module, SrcFile, Errors, Warnings) ->
    Msg = [
        format_errors(SrcFile, Errors, Warnings)
    ],
    error_logger:info_msg(lists:flatten(Msg)).


%% @private Print error messages in a pretty and user readable way.
format_errors(File, Errors, Warnings) ->
    AllErrors1 = lists:sort(lists:flatten([X || {_, X} <- Errors])),
    AllErrors2 = [{Line, "Error", Module, Description} || {Line, Module, Description} <- AllErrors1],
    AllWarnings1 = lists:sort(lists:flatten([X || {_, X} <- Warnings])),
    AllWarnings2 = [{Line, "Warning", Module, Description} || {Line, Module, Description} <- AllWarnings1],
    Everything = lists:sort(AllErrors2 ++ AllWarnings2),
    F = fun({Line, Prefix, Module, ErrorDescription}) ->
        Msg = Module:format_error(ErrorDescription),
        io_lib:format("~s:~p: ~s: ~s~n", [File, Line, Prefix, Msg])
    end,
    [F(X) || X <- Everything].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------