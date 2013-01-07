%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(cc_file_poller).

-behaviour(gen_server).
-author('uaforum1@googlemail.com').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
%% External exports
%% cc_timer interface which has to implemented by the timer clients
-export([time_triggered/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).

-define(DEBUG(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
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
    {ok, #state{last_poll_time = new_poll_time(date(), time())}}.

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
	?DEBUG("cc_file_poller was triggered"),
	{Directory, Compiled_Regex} = get_parameter(),
	{Files, NewState} = get_new_files(Directory, Compiled_Regex, State),
	send_cc_controller(Files),
    {noreply, NewState}.
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
get_new_files(Directory, Compiled_Regex, _State=#state{last_poll_time=Last_poll_time}) ->

    {ok, Files} = file:list_dir(Directory),
	New_state = #state{last_poll_time=new_poll_time(date(), time())},
    FilteredFiles = lists:map(
        fun(X) -> filename:join([Directory,X]) end,
        lists:filter(
            fun(Y) ->
                re:run(Y,Compiled_Regex,[{capture,none}]) == match end,
            Files
        )
    ),
    NewFiles = lists:filter (
        fun(Filename) ->
            {ok, FileInfo} = file:read_file_info(Filename),
            calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime) > Last_poll_time
        end,
        FilteredFiles
    ),				   
    {NewFiles, New_state}.
	
get_parameter() ->
	Directory = get_polling_dir(),
	Regex = get_files_regex(),
	{ok, Compiled_Regex} = re:compile(Regex),
	{Directory, Compiled_Regex}.

get_polling_dir() ->
	case erlbuild:get_env(polling_dir) of
		{ok, Value} -> Value;
		undefined -> []
	end.
get_files_regex() ->
	case erlbuild:get_env(files_regex) of
		{ok, Value} -> Value;
		undefined -> []
	end.
%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
send_cc_controller([]) ->
	?DEBUG("no files are available for processing");	
send_cc_controller(Files) ->
	cc_controller:process_files(Files).

%% --------------------------------------------------------------------
%%% create new poll time
%% --------------------------------------------------------------------
new_poll_time(Date, Time) ->
	calendar:datetime_to_gregorian_seconds({Date, Time}).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_parameter_test() ->
	application:load(erlbuild),
	?assertMatch({"./src", {re_pattern,0,0,_}}, get_parameter()).
	
-endif.