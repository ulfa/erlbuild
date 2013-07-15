%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :  
%%% Created : 
%%% -------------------------------------------------------------------
-module(cc_file_poller).

-behaviour(gen_server).
-author('uaforum1@googlemail.com').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
-include("../include/erlbuild.hrl").
%% --------------------------------------------------------------------
%% External exports
%% cc_timer interface which has to implemented by the timer clients
-export([time_triggered/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).

-record(state, {last_poll_time}).
%% ====================================================================
%% External functions
%% ====================================================================
time_triggered(Args) ->
	gen_server:cast(?MODULE, {time_triggered, Args}).
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
    {ok, #state{last_poll_time = new_poll_time(erlang:localtime())}}.
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
handle_cast({time_triggered, _Args}, State) ->
	process_src_files(State),
	process_dtl_files(State),	
    {noreply, #state{last_poll_time = new_poll_time(erlang:localtime())}}.
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
%% Get all changed files for a given directory.
%% 1. read all files of given directory
%% 2. filter all files where the mtime < localtime
%% --------------------------------------------------------------------
get_new_files(Directory, Compiled_Regex, _State=#state{last_poll_time = Last_poll_time}) ->
    Files = list_dir(Directory),
    FilteredFiles = lists:map(
        fun(X) -> filename:join([Directory, X]) end,
        lists:filter(
            fun(Y) ->
                re:run(Y,Compiled_Regex,[{capture,none}]) == match end,
            Files
        )
    ),	
    lists:filter (
        fun(Filename) ->
            {ok, FileInfo} = file:read_file_info(Filename),											
            calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime) >= Last_poll_time 
        end,				
        FilteredFiles
    ).	
%% --------------------------------------------------------------------
%% Get all files of a give directory
%% --------------------------------------------------------------------
list_dir(Directory) when is_list(Directory)->
	list_dir(file:list_dir(Directory));
list_dir({ok, Files}) ->
	Files;
list_dir({error, Reason}) ->
	error_logger:error_msg("Please, configure the path to the sources : ~p",[Reason]),
	[].
%% --------------------------------------------------------------------
%% 
%% --------------------------------------------------------------------	
process_src_files(State) ->
	Files = get_new_files(get_src_dir(), get_regex(".*.erl$"), State),
	send_cc_compiler(src, Files).
process_dtl_files(State) ->
	Files = get_new_files(get_dtl_dir(), get_regex(".*.dtl$"), State),
	send_cc_compiler(dtl, Files).
%% --------------------------------------------------------------------
%% Get the directory where the erlang sources are stored
%% --------------------------------------------------------------------	
get_src_dir() ->
	case erlbuild:get_env(src_dir) of
		{ok, Value} -> Value;
		undefined -> "./src"
	end.
%% --------------------------------------------------------------------
%% Get the directory where the erlydtl sources are stored
%% --------------------------------------------------------------------	
get_dtl_dir() ->
	case erlbuild:get_env(dtl_dir) of
		{ok, Value} -> Value;
		undefined -> "./templates"
	end.
%% --------------------------------------------------------------------
%% 
%% --------------------------------------------------------------------	
get_regex(Regex) ->
	{ok, Compiled_Regex} = re:compile(Regex),
	Compiled_Regex.	
%% --------------------------------------------------------------------
%% 
%% --------------------------------------------------------------------
send_cc_compiler(_Type,[]) ->
	ok;
send_cc_compiler(Type, Files) ->
	%%?DEBUG(Files),
	cc_compiler:process_files(Type, Files).
%% --------------------------------------------------------------------
%%% create new poll time
%% --------------------------------------------------------------------
new_poll_time(DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_new_files_test() ->
	Filename = "./testdir/ee.erl",
	{ok, FI} = file:read_file_info(Filename),			
	ok = file:write_file_info(Filename, FI#file_info{mtime={{2020, 12,31}, {23,59,59}}, ctime={{2020, 12,31}, {23,59,59}}}),
	Files = get_new_files("./testdir", get_regex(".*.erl$"), #state{last_poll_time = calendar:datetime_to_gregorian_seconds(erlang:localtime())}),
	?assertEqual(1, length(Files)).
-endif.