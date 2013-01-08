%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com'
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(cc_reloader).

-behaviour(gen_server).
-author('uaforum1@googlemail.com').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/erlbuild.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([reload_modules/0]).
%% cc_timer interface which has to implemented by the timer clients
-export([time_triggered/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, is_not_system_module/1]).

-record(state, {}).
%% ====================================================================
%% External functions
%% ====================================================================
reload_modules() ->
	gen_server:cast(?MODULE, {reload, []}).
time_triggered([]) ->
	gen_server:cast(?MODULE, {time_triggered, []}).
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
handle_cast({reload, _Revision}, State) ->	
	reloadmodules(),
    {noreply, State};
handle_cast({time_triggered, []}, State) ->
	error_logger:info_msg("got an time triggered message ~n"),
	reloadmodules(),
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
%% Func: reloadmodules/0
%% Purpose: Wir ueberpruefen alle geladenen Module, ob sie beam Files 
%% sind und ob sie aelter als die nicht geladenen Module sind.
%% Diese neuen Module werden dann mittles c:l geladen.
%% Returns: 
%% --------------------------------------------------------------------
reloadmodules() ->
	[load_module(Module) || {Module, File} <- code:all_loaded(), is_not_system_module(Module), is_beamfile(File), is_old(Module,File)].
%% --------------------------------------------------------------------
%% Func: 
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
is_old(Module,File) ->
	%%?DEBUG(File),
	loaded_time(Module) < not_yet_loaded_time(File).
%% --------------------------------------------------------------------
%% Func: loaded_time/1
%% Purpose: Diese Funtion ermittelt die Zeit, wann ein Modul compiliert
%%          wurde. 
%% Returns: {yyyy,mm,dd,hh,mm,sec}
%% --------------------------------------------------------------------
loaded_time(Module) ->
	proplists:get_value(time, Module:module_info(compile)).
%% --------------------------------------------------------------------
%% Func: not_yet_load_time/1
%% Purpose: Liest die Compile Zeit des Beam Files aus. Diese wird mittels
%% der beam_lib ermittelt
%% Returns: {yyyy,mm,dd,hh,mm,sec}
%% --------------------------------------------------------------------
not_yet_loaded_time(File) ->
	{ok,{_,[{_,I}]}} = beam_lib:chunks(File,[compile_info]),
	proplists:get_value(time,I).
%% --------------------------------------------------------------------
%% Func: s_beamfile/1
%% Purpose: ueberprueft, ob es sich bei der Datei um ein beam File handelt.
%% Das Eingabeformat siehht wie folgt aus:
%% "/Users/ua/projekte/erlang/lilly/ebin/login_srv.beam" 
%% Returns: true | false
%% --------------------------------------------------------------------
is_beamfile(File) ->
	ok =:= element(1,file:read_file_info(File)) andalso ".beam" =:= filename:extension(File).
%% --------------------------------------------------------------------
%% Func: load_module
%% Purpose: 
%% Returns: 
%% --------------------------------------------------------------------
load_module(Module) ->
	?DEBUG(Module),
	code:purge(Module), 
	case code:load_file(Module) of
		{module, Loaded_Module} -> error_logger:info_msg("loaded Module : ~p~n", [Loaded_Module]),
									run_test(Module);
		{error, Reason} -> error_logger:info_msg("can't load Module : ~p with Reason : ~p ~n", [Module, Reason])
	end.

run_test(Module) ->
	case lists:keyfind(test, 1, Module:module_info(exports)) of
		false -> [];
		{test, 0} -> Module:test();
		_ -> []
	end. 


is_not_system_module(Module) ->
	[] =:= [X || X <- get_system_modules(), X =:= Module].
get_system_modules() ->
		[
        appmon,
        asn1,
        common_test,
        compiler,
        crypto,
        debugger,
        dialyzer,
        docbuilder,
        edoc,
        erl_interface,
        erts,
        et,
        eunit,
        gs,
        hipe,
        inets,
        inets,
        inviso,
        jinterface,
        kernel,
        mnesia,
        observer,
        orber,
        os_mon,
        parsetools,
        percept,
        pman,
        reltool,
        runtime_tools,
        sasl,
        snmp,
        ssl,
        stdlib,
        syntax_tools,
        test_server,
        toolbar,
        tools,
        tv,
        webtool,
        wx,
        xmerl,
        zlib
    ].
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.	