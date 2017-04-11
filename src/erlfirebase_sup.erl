
-module(erlfirebase_sup).

-behaviour(supervisor).

-include("erlfirebase.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    % mnesia:create_schema([node()]),
    % mnesia:change_table_copy_type(schema, node(), disc_copies), 
    % mnesia:start(),
    % mnesia:create_table(request,
    % [ {disc_copies, [node()] },
    %      {attributes,      
    %         record_info(fields, request)} ]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(erlfirebase, worker)]} }.

