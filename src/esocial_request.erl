-module(esocial_request).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

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
start_link() ->  % {{{2
    Workers = application:get_env(esocial, request_workers, 10),
    hottub:start_link(request, Workers, gen_server, start_link, [?MODULE, [], []]).

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
init([]) ->  % {{{2
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->  % {{{2
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
handle_cast({From, URL}, State) ->  % {{{2
    lager:debug("Requesting URL: ~p from pid: ~p", [URL, From]),
    Responce = hackney:get(URL, [], [], [{follow_rediret, true}]),
    Map = decode_responce(Responce),
    gen_server:reply(From, Map),
    {noreply, State};
handle_cast(_Msg, State) ->  % {{{2
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
handle_info(_Info, State) ->  % {{{2
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
terminate(_Reason, _State) ->  % {{{2
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->  % {{{2
    {ok, State}.

%%%===================================================================
%%% Internal functions  % {{{1
%%%===================================================================
decode_responce({ok, 200, _, Ref}) ->  % {{{2
    Body = hackney:body(Ref),
    decode_body(Body);
decode_responce({ok, Code, _, Ref}) ->  % {{{2
    Body = hackney:body(Ref),
    lager:warning("Request returned wrong code: ~p ~p", [Code, Body]),
    #{};
decode_responce({error, Reason}) ->  % {{{2
    lager:warning("Request error: ~p", [Reason]),
    #{}.

decode_body({ok, Body}) ->  % {{{2
    try
        jsx:decode(Body, [return_maps])
    catch 
        error:badarg -> 
            lager:warning("JSON decode: ~p", [Body]),
            #{}
    end;
decode_body({error, Reason}) ->  % {{{2
    lager:warning("Body decode error: ~p", [Reason]),
    #{}.
