-module(erlang_v8_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).
-export([create_pool/3, delete_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions

start_link() ->
    start_link([], global).

start_link(Pools, GlobalOrLocal) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools, GlobalOrLocal]).

%% ===================================================================
%% @doc create new pool.
%% @end
%% ===================================================================
-spec(create_pool(PoolName::atom(), Size::integer(), Options::[tuple()]) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Options) ->
    PoolSpec = {PoolName, {poolboy, start_link, [[{name,{global, PoolName}},
                                                  {worker_module, erlang_v8_vm},
                                                  {size, Size},
                                                  {max_overflow, 10}]
                                                  ++ Options
                                                ]},
                permanent, 5000, worker,
                [poolboy, erlang_v8_vm]},

    supervisor:start_child(?MODULE, PoolSpec).

%% ===================================================================
%% @doc delet pool and disconnected to Redis.
%% @end
%% ===================================================================
-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).

delete_pool(PoolName) ->
    supervisor:terminate_child(?MODULE, PoolName),
    supervisor:delete_child(?MODULE, PoolName).

%% Supervisor callbacks

init([Pools, GlobalOrLocal]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
                                  Args = [{name, {GlobalOrLocal, PoolName}},
                                          {worker_module, erlang_v8_vm}]
                                      ++ PoolConfig,
                                  {PoolName, {poolboy, start_link, [Args]},
                                   Restart, Shutdown, Type, []}
                          end, Pools),

    {ok, {SupFlags, PoolSpecs}}.
