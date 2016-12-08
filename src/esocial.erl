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
         captcha/2,
         profile/1,
         profile/2,
         profiles/2,
         playlists/1,
         playlists/2,
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
start_link() -> % {{{1
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec connect(platform(), binary()) -> binary(). % {{{1
connect(Platform, RedirectURI) ->
    Module = platform_to_module(Platform),
    Module:connect(RedirectURI, ["*"]).

-spec connect(platform(), binary(), Scopes) -> binary() when % {{{1
      Scopes :: [Scope],
      Scope :: iodata().
connect(Platform, RedirectURI, Scopes) ->
    Module = platform_to_module(Platform),
    Module:connect(RedirectURI, Scopes).

-spec auth(Platform, Code, RedirectURI) -> handler() when % {{{1
      Platform :: platform(),
      Code :: binary(),
      RedirectURI :: binary().
auth(Platform, Code, RedirectURI) ->
    Module = platform_to_module(Platform),
    Module:auth(Code, RedirectURI).


-spec captcha(handler(), Captcha) -> handler() when % {{{1
      Captcha :: any().
captcha(#esocial{module=Module, user_id=Id, token=Token}=Handler, Captcha) ->
    Module:captcha(Handler, Captcha).

-spec profile(handler()) -> profile(). % {{{1
profile(#esocial{module=Module, user_id=Id, token=Token}=Handler) ->
    Module:profile(Handler, Id).

-spec profile(handler(), esocial_id()) -> profile(). % {{{1
profile(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:profile(Handler, Id).

-spec profiles(handler(), [esocial_id()]) -> [profile()]. % {{{1
profiles(#esocial{module=Module, token=Token}=Handler, IDs) ->
    Module:profiles(Handler, IDs).

-spec playlists(handler()) -> [playlist()]. % {{{1
playlists(#esocial{module=Module, token=Token, user_id=UID}=Handler) ->
    Module:playlists(Handler, UID).

-spec playlists(handler(), [esocial_id()]) -> [playlist()]. % {{{1
playlists(#esocial{module=Module, token=Token}=Handler, IDs) ->
    Module:playlists(Handler, IDs).

-spec playlist(handler(), esocial_id()) -> playlist(). % {{{1
playlist(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:playlist(Handler, Id).

-spec track(handler(), esocial_id()) -> track(). % {{{1
track(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:track(Handler, Id).

-spec tracks(handler(), [esocial_id()]) -> [track()]. % {{{1
tracks(#esocial{module=Module, token=Token}=Handler, Ids) ->
    Module:tracks(Handler, Ids).

-spec user_tracks(handler(), esocial_id()) -> [track()]. % {{{1
user_tracks(#esocial{module=Module, token=Token}=Handler, Id) ->
    Module:user_tracks(Handler, Id).

-spec user_tracks(handler()) -> [track()]. % {{{1
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
init([]) -> % {{{1
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
handle_cast(_Msg, State) -> % {{{1
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
handle_info(_Info, State) -> % {{{1
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
terminate(_Reason, _State) -> % {{{1
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> % {{{1
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
platform_to_module(M) -> % {{{1
    list_to_atom("esocial_" ++ atom_to_list(M)).

