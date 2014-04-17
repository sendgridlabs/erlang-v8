-module(erlang_v8_vm).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start/0]).
-export([start_link/1]).

-export([stop/1]).
-export([reset/1]).
-export([restart/1]).

-export([eval/2]).
-export([eval/3]).
-export([call/3]).
-export([call/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(EXECUTABLE, "erlang_v8").
-define(SPAWN_OPTS, [{packet, 2}, binary]).
-define(DEFAULT_TIMEOUT, 5000).

-define(OP_EVAL, 0).
-define(OP_RESET, 2).

-record(state, {
        port,
        monitor_pid
    }).

%% External API

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start() ->
    gen_server:start(?MODULE, [], []).

eval(Pid, Source) ->
    eval(Pid, Source, ?DEFAULT_TIMEOUT).

eval(Pid, Source, Timeout) ->
    gen_server:call(Pid, {eval, Source, Timeout}, Timeout + 1000).

call(Pid, FunctionName, Args) ->
    call(Pid, FunctionName, Args, ?DEFAULT_TIMEOUT).

call(Pid, FunctionName, Args, Timeout) ->
    gen_server:call(Pid, {call, FunctionName, Args, Timeout}, Timeout + 1000).

reset(Pid) ->
    gen_server:call(Pid, reset).

restart(Pid) ->
    gen_server:call(Pid, restart).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% Callbacks

init(_Args) ->
    State = start_port(#state{}),
    {ok, State}.

handle_call({call, FunctionName, Args, Timeout}, From, State) ->
    %% TODO: call should be a special op and decoding should be done in 
    %% the cc wrapper.
    SerializedArgs = jiffy:encode(Args),
    Source = <<FunctionName/binary, ".apply(null, JSON.parse('",
               SerializedArgs/binary ,"'));">>,
    handle_call({eval, Source, Timeout}, From, State);

handle_call({eval, Source, Timeout}, _From, #state{port = Port} = State) ->
    case eval_js(Port, Source, Timeout) of
        {ok, Response} ->
            {reply, {ok, Response}, State};
        {error, timeout} ->
            {reply, {error, timeout}, start_port(kill_port(State))};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(reset, _From, #state{port = Port} = State) ->
    Port ! {self(), {command, <<?OP_RESET:8>>}},
    {reply, ok, State};

handle_call(restart, _From, State) ->
    {reply, ok, start_port(close_port(State))};

handle_call(stop, _From, State) ->
    {stop, normal, closed, State};

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close_port(State),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

%% @doc Close port gently.
close_port(#state{monitor_pid = Pid, port = Port} = State) ->
    catch port_close(Port),
    Pid ! demonitor,
    State#state{monitor_pid = undefined, port = undefined}.

%% @doc Close port and attempt to kill the OS process.
kill_port(#state{monitor_pid = Pid, port = Port} = State) ->
    catch port_close(Port),
    Pid ! kill,
    State#state{monitor_pid = undefined, port = undefined}.

%% @doc Start port and port monitor.
start_port(State) ->
    Executable = filename:join(priv_dir(), ?EXECUTABLE),
    Port = open_port({spawn_executable, Executable}, ?SPAWN_OPTS),
    monitor_port(State#state{port = Port}).

%% @doc Kill active port monitor before starting a new process.
monitor_port(#state{monitor_pid = Pid} = State) when is_pid(Pid) ->
    Pid ! demonitor,
    monitor_port(State#state{monitor_pid = undefined});

%% @doc Start a process that monitors the port (and parent process) and kills
%% the actual OS process when things go south.
monitor_port(#state{port = Port} = State) ->
    Parent = self(),
    Pid = spawn(fun() ->
        {os_pid, OSPid} = erlang:port_info(Port, os_pid),
        MRef = erlang:monitor(process, Parent),
        receive 
            demonitor ->
                erlang:demonitor(MRef);
            kill ->
                os_kill(OSPid);
            {'DOWN', _Ref, process, Parent, _Reason} ->
                os_kill(OSPid)
        end
    end),
    State#state{monitor_pid = Pid}.

%% @doc Kill OS process.
os_kill(OSPid) ->
    os:cmd(io_lib:format("kill -9 ~p", [OSPid])).

%% @doc Evaluate source on port
eval_js(Port, Source, Timeout) ->
    Port ! {self(), {command, <<?OP_EVAL:8, Source/binary>>}},
    receive
        {Port, {data, <<_:8, "undefined">>}} ->
            {ok, undefined};
        {Port, {data, <<0:8, Response/binary>>}} ->
            {ok, jiffy:decode(Response)};
        {Port, {data, <<1:8, Response/binary>>}} ->
            {[{<<"error">>, Reason}]} = jiffy:decode(Response),
            {error, Reason};
        {Port, Error} ->
            %% TODO: we should probably special case here.
            {error, Error}
        after Timeout ->
            {error, timeout}
    end.

%% @doc Return the path to the application's priv dir (assuming directory
%% structure is intact).
priv_dir() ->
    filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                  "priv").
