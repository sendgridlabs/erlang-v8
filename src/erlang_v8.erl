-module(erlang_v8).


%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

-export([start_vm/0]).
-export([start_vm/1]).
-export([stop_vm/1]).

-export([reset_vm/1, restart_vm/1]).

-export([start/0, stop/0]).
-export([create_pool/2, create_pool/3, delete_pool/1]).
-export([eval/2, eval/3]).
-export([call/3, call/4]).
-export([eval_js/2, eval_js/3]).
-export([map_reduce/3, map_reduce/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_vm() ->
    start_vm([]).

start_vm(Opts) ->
    erlang_v8_vm:start_link(Opts).

stop_vm(Pid) ->
    erlang_v8_vm:stop(Pid).


start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ===================================================================
%% @doc create new pool.
%% @end
%% ===================================================================
-spec(create_pool(PoolName::atom(), Size::integer()) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size) ->
    erlang_v8_sup:create_pool(PoolName, Size).

-spec(create_pool(PoolName::atom(), Size::integer(), Args::[tuple()]) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Args) ->
    erlang_v8_sup:create_pool(PoolName, Size, Args).

%% ===================================================================
%% @doc delet pool.
%% @end
%% ===================================================================
-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).

delete_pool(PoolName) ->
    erlang_v8_sup:delete_pool(PoolName).


%%--------------------------------------------------------------------
%% @doc
%% Executes the given command in the specified connection. The
%% command must be a valid Redis command and may contain arbitrary
%% data which will be converted to binaries. The returned values will
%% always be binaries.
%% @end
%%--------------------------------------------------------------------
reset_vm(Pid) ->
    erlang_v8_vm:reset(Pid).

restart_vm(Pid) ->
    erlang_v8_vm:restart(Pid).

eval(Pid, Source) ->
    erlang_v8_vm:eval(Pid, Source).

eval(Pid, Source, Timeout) ->
    erlang_v8_vm:eval(Pid, Source, Timeout).

call(Pid, FunctionName, Args) ->
    erlang_v8_vm:call(Pid, FunctionName, Args).

call(Pid, FunctionName, Args, Timeout) ->
    erlang_v8_vm:call(Pid, FunctionName, Args, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Executes the given command in the specified connection. The
%% command must be a valid Redis command and may contain arbitrary
%% data which will be converted to binaries. The returned values will
%% always be binaries.
%% @end
%%--------------------------------------------------------------------
-spec eval_js(PoolName::atom(), Source::binary()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

eval_js(PoolName, Source) ->
    eval_js(PoolName, Source, ?TIMEOUT).

-spec eval_js(PoolName::atom(), Source::binary(), Timeout::integer()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

eval_js(PoolName, Source, Timeout) ->
    poolboy:transaction(PoolName, fun(Pid) ->
                                          erlang_v8_vm:reset(Pid),
                                          erlang_v8_vm:eval(Pid, Source, Timeout)
                                  end).

%%--------------------------------------------------------------------
%% @doc
%% Executes the given command in the specified connection. The
%% command must be a valid Redis command and may contain arbitrary
%% data which will be converted to binaries. The returned values will
%% always be binaries.
%% @end
%%--------------------------------------------------------------------
-spec map_reduce(PoolName::atom(), Source::binary(), Args::[tuple()]) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

map_reduce(PoolName, Source, Args) ->
    map_reduce(PoolName, Source, Args, ?TIMEOUT).

-spec map_reduce(PoolName::atom(), Source::binary(), Args::[tuple()], Timeout::integer()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

map_reduce(PoolName, Source, Args, Timeout) ->
    poolboy:transaction(PoolName, fun(Pid) ->
                                          SerializedArgs = jsx:encode(Args),
                                          SourceResult = <<"(function(){ var data = arguments[0]; ", Source/binary, "}).apply(null, ",
                                                     SerializedArgs/binary ,");">>,
                                          erlang_v8_vm:eval(Pid, SourceResult, Timeout)
                                  end).
