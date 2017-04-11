-module(erlfirebase).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("erlfirebase.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
		 push/3,
		 push/4,
		 push/5
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

push(DeviceId, Title, Body) ->
	gen_server:cast(?MODULE, {push, {DeviceId, Title, Body, [], undefined}}).

push(DeviceId, Title, Body, Opts) ->
	gen_server:cast(?MODULE, {push, {DeviceId, Title, Body, Opts, undefined}}).

push(DeviceId, Title, Body, Opts, NotificationPID) ->
	gen_server:cast(?MODULE, {push, {DeviceId, Title, Body, Opts, NotificationPID}}).
	
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {apikey::binary()}).

init([]) ->
    {ok, #state{}}.

handle_cast(Msg, {state,undefined} = State) ->
    case application:get_env(?APPNAME, apikey) of
        {ok, ApiKey} when is_binary(ApiKey) -> handle_cast(Msg, #state{apikey=ApiKey});
        {ok, ApiKey} when is_list(ApiKey) -> Key = list_to_binary(ApiKey),
            handle_cast(Msg, #state{apikey=Key});
        _ -> lager:error("API key is invalid or not provided"),
            {noreply, State}
    end;

handle_cast({Op, {DeviceId, Title, Body, Opts, NotificationPID}}, 
            #state{apikey=ApiKey}=State) ->
    Msg = [{<<"title">>, Title},
        {<<"body">>,Body}] ++ Opts,
    MessBody = jsx:minify(jsx:encode([{<<"notification">>, Msg},
                {<<"to">>, DeviceId}
            ])),
    ApiKeyString = binary_to_list(State#state.apikey),
    case catch httpc:request(post, 
        {?URL, [{"Authorization", "key=" ++ ApiKeyString}], "application/json", 
          binary_to_list(MessBody)}, [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _, JSON}} ->
            lager:info("firebase accepted: ~p", [JSON]);
        {_, ErrMsg} -> 
            lager:error("HTTP error ~p",[ErrMsg])
    end,
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
unixtime() ->
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 62167219200.

