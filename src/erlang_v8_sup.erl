-module(erlang_v8_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([create_pool/2, delete_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% @doc create new pool.
%% @end
%% ===================================================================
-spec(create_pool(PoolName::atom(), Size::integer()) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size) ->
    PoolSpec = {PoolName, {poolboy, start_link, [[{name,{global, PoolName}},
                                                  {worker_module, erlang_v8_vm},
                                                  {size, Size},
                                                  {max_overflow, 10}]
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

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.
