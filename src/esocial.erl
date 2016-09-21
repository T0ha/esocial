%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2016, ins
%%% @doc
%%%
%%% @end
%%% Created : 2016-08-30 15:22:18.651553
%%%-------------------------------------------------------------------
-module(esocial).

-behaviour(gen_server).

-include("include/esocial.hrl").


%% API
-export([
         start_link/0,
         connect/2,
         connect/3,
         auth/3,
         profile/1,
         profile/2,
         profiles/2,
         track/2,
         tracks/2,
         user_tracks/2,
         user_tracks/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec connect(platform(), binary()) -> binary().
connect(Platform, RedirectURI) ->
    Module = platform_to_module(Platform),
    Module:connect(RedirectURI, ["*"]).

-spec connect(platform(), binary(), Scopes) -> binary() when
      Scopes :: [Scope],
      Scope :: iodata().
connect(Platform, RedirectURI, Scopes) ->
    Module = platform_to_module(Platform),
    Module:connect(RedirectURI, Scopes).

-spec auth(Platform, Code, RedirectURI) -> handler() when
      Platform :: platform(),
      Code :: binary(),
      RedirectURI :: binary().
auth(Platform, Code, RedirectURI) ->
    Module = platform_to_module(Platform),
    Module:auth(Code, RedirectURI).

-spec profile(handler()) -> profile().
profile(#esocial{module=Module, user_id=Id, token=Token}=Handler) ->
    Module:profile(Handler, Id).

-spec profile(handler(), esocial_id()) -> profile().
profile(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:profile(Handler, Id).

-spec profiles(handler(), [esocial_id()]) -> [profile()].
profiles(#esocial{module=Module, token=Token}=Handler, IDs) ->
    Module:profiles(Handler, IDs).

-spec playlist(handler(), esocial_id()) -> playlist().
playlist(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:playlist(Handler, Id).

-spec track(handler(), esocial_id()) -> track().
track(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:track(Handler, Id).

-spec tracks(handler(), [esocial_id()]) -> [track()].
tracks(#esocial{module=Module, token=Token}=Handler, Ids) ->
    Module:tracks(Handler, Ids).

-spec user_tracks(handler(), esocial_id()) -> [track()].
user_tracks(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:user_tracks(Handler, Id).

-spec user_tracks(handler()) -> [track()].
user_tracks(#esocial{module=Module, user_id=Id, token=Token}=Handler) ->
    Module:user_tracks(Handler, Id).

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
init([]) ->
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
platform_to_module(M) ->
    list_to_atom("esocial_" ++ atom_to_list(M)).

