-module(esocial_vk).

-behaviour(gen_server).

%% API functions
-export([
         start_link/1,
         call/2,
         get_access_token/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          app_id = 0 :: non_neg_integer(),
          secret = <<>> :: binary(),
          config = [] :: [proplists:property()]
               }).
-define(BASE_URL, <<"https://api.vk.com">>).
-define(BASE_AUTH_URL, <<"https://oauth.vk.com">>).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

-spec call(Method, Args) -> Response when
      Method :: iodata(),
      Args :: map() | [proplists:property()],
      Response :: map() | [proplists:property()].
call(Method, Args) -> 
    gen_server:call(?MODULE, {call, Method, Args}, infinity).

-spec get_access_token(Code, Redirect) -> map() when 
      Code :: iodata(),
      Redirect :: binary().
get_access_token(Code, Redirect) ->
    gen_server:call(?MODULE, {auth, Code, Redirect}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Config]) ->
    AppID = proplists:get_value(app_id, Config, 0),
    Secret = proplists:get_value(secret, Config, <<>>),
    lager:info("Starting vk handler for ~p", [ AppID ]),
    {ok, #state{app_id=AppID, config=Config, secret=Secret}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({auth, Code, Redirect}, From, #state{app_id=AppID, secret=Secret}=State) ->
    Args = [
            {client_id, AppID},
            {client_secret, Secret},
            {redirect_uri, Redirect},
            {code, Code}
           ],
    URL = hackney_url:make_url(?BASE_AUTH_URL, [<<"access_token">>], Args),
    hottub:cast(request, {From, URL}),
    {noreply, State};
handle_call({call, Method, Args}, From, #state{app_id=AppID}=State) ->
    URL = hackney_url:make_url(?BASE_URL, [<<"method">>, Method], Args),
    hottub:cast(request, {From, URL}),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
