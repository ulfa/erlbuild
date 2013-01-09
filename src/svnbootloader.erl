%%% -------------------------------------------------------------------
%%% Author  : ua
%%% Description :
%%%
%%% Created : Jan 7, 2010
%%% -------------------------------------------------------------------
-module(svnbootloader).

-behaviour(gen_server).
-author('uangermann@googlemail.com').
-vsn(1.0).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([update/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, run_eunit_test1/0]).


-record(state, {}).

-define(LILLY_DIR, '/Users/ua/projekte/erlang/lilly').
-define(SVN_UPDATE,'svn update --non-interactive').
%% ====================================================================
%% External functions
%% ====================================================================
update() ->
	gen_server:call(?MODULE, {update}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	start_timer(),	
	case file:set_cwd(get_build_dir()) of
		ok -> {ok, #state{}} ; 
    	{error,Reason} -> {error,Reason}
	end.

get_build_dir() ->
	{ok, Value} = application:get_env(erlbuild, build_dir),
	io:format("Build_dir : ~p ~n" , [Value]),
	Value.

%% timer nach 1 Minuten
start_timer() ->
	erlang:send_after(get_timer() * 60 * 1000, self(), timeout).

get_timer() ->
	{ok, Value} = application:get_env(erlbuild, timer),
	%%io:format("time to next run : ~p minutes~n" , [Value]),
	Value.
	
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
handle_call({update}, _From, State) ->
	svn_update(),
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
	svn_update(),
	start_timer(),
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
%% Func: svn_update/0
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
svn_update() ->
	Update_Out = os:cmd(?SVN_UPDATE),
	case check_svn_result(Update_Out) of
		build -> make(get_revision(Update_Out)),
				 run_eunit(Update_Out);
		_	  -> ok %%io:format("no build needed~n")
	end.
%% --------------------------------------------------------------------
%% Func: check_svn_result/1
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
check_svn_result(Svn_Result) ->
	Liste = string:tokens(Svn_Result, "\n"),
	case erlang:length(Liste) of
		1 -> no_build;
		_ -> build
	end.
%% --------------------------------------------------------------------
%% Func: check_svn_result/1
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
get_revision(Svn_Result) ->
	Svn_result = string:to_lower(Svn_Result),
	Start = string:str(Svn_result, "revision"),
	Ende  = string:rstr(Svn_result,".\n"),
	string:substr(Svn_result, Start, (Ende - Start)).
%% --------------------------------------------------------------------
%% Func: make/0
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
make(Revision) ->
	case make:all() of 
		error -> io:format("ERROR. Now we have to send a mail, but it is not implemented yet! ~n");
		_ -> trigger_code_reloading(Revision)
	end.
%% --------------------------------------------------------------------
%% Func: run_eunit/1
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
run_eunit(Svn_Result) ->
	%%io:format("1. run_eunit ~p~n", [Svn_Result]),
	Liste = string:tokens(Svn_Result, "\n"),
	Liste1 = lists:delete(lists:last(Liste), Liste),
	
	io:format("2. run_eunit ~p~n", [Liste1]).
%% --------------------------------------------------------------------
%% Func: trigger_code_reloading/0
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
trigger_code_reloading(Revision) ->
	io:format("send message to code_reloader for : ~p~n", [Revision]),
	code_reloader:reload_modules(Revision),
	ok.
%%
%% Test Functions
%%
get_revision_test() ->
	?assertEqual("revision 182", get_revision("U    src/svnbootloader.erl\nUpdated to Revision 182.\n")),
	?assertEqual("revision 182", get_revision("U    src/svnbootloader.erl\nUpdated to revision 182.\n")).

run_eunit_test1() ->
	run_eunit("U    src/svnbootloader.erl\nUpdated to revision 182.\n"),
	["U    src/svnbootloader.erl", "Updated to revision 182."].

	

