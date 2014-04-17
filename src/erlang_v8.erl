-module(erlang_v8).

%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

-export([start/0, stop/0]).

-export([create_pool/2, delete_pool/1]).
-export([eval/2, eval/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

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
    erlang_v8_sup:create_pool(PoolName, Size, []).

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
-spec eval(PoolName::atom(), Source::binary()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

eval(PoolName, Source) ->
    eval(PoolName, Source, ?TIMEOUT).

-spec eval(PoolName::atom(), Source::binary(), Timeout::integer()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

eval(PoolName, Source, Timeout) ->
    poolboy:transaction(PoolName, fun(Pid) ->
                                          erlang_v8_vm:eval(Pid, Source, Timeout)
                                  end).