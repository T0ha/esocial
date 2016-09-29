-module(esocial_vk).

-behaviour(gen_server).

-include("include/esocial.hrl").
%% API functions
-export([
         start_link/1,
         connect/2,
         auth/2,
         profile/2,
         profiles/2,
         playlists/2,
         track/2,
         tracks/2,
         user_tracks/2,
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
          captcha ::  [proplists:property()],
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
start_link(Config) -> % {{{1
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).


-spec connect(binary(), Scopes) -> binary() when % {{{1
      Scopes :: [Scope],
      Scope :: iodata().
connect(RedirectURI, Scopes) ->
    gen_server:call(?MODULE, {connect, RedirectURI, Scopes}).

-spec auth(Code, RedirectURI) -> handler() when % {{{1
      Code :: binary(),
      RedirectURI :: binary().
auth(Code, RedirectURI) ->
    case get_access_token(Code, RedirectURI) of
        #{<<"user_id">> := UID,
          <<"access_token">> := Token} ->
            {ok, 
            #esocial{
               module=?MODULE,
               platform=vk,
               user_id=UID,
               token=Token
              }};
        Any -> {error, Any}
    end.
        
-spec profile(handler(), esocial_id()) -> profile(). % {{{1
profile(Handler, Id) ->
    case profiles(Handler, [Id]) of
        [#esocial_profile{}=P] -> P;
        Any -> Any
    end.

-spec profiles(handler(), [esocial_id()]) -> [profile()]. % {{{1
profiles(#esocial{token=Token}=Handler, IDs) ->
    Method = "users.get",
    BinIDs = string:join([integer_to_list(ID) || ID <- IDs], ","),
    Args = [
            {user_ids, BinIDs},
            {fields, "photo_50,country,bdate"},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, Profiles} ->
            lists:map(fun(#{
                        <<"uid">> := PID,
                        <<"first_name">> := Name,
                        <<"last_name">> := Surname,
                        <<"country">> := Country,
                        <<"bdate">> :=BirthDate,
                        <<"photo_50">> := Photo
                       }) ->
                              URL = <<"https://vk.com/id", (integer_to_binary(PID))/bytes>>,
                              #esocial_profile{
                                 id = PID,
                                 display_name = <<Name/bytes, " ", Surname/bytes>>,
                                 birthdate  = BirthDate,
                                 country = Country,
                                 profile_uri = URL,
                                 photo = Photo
                                }
                      end,
                      Profiles);
        Any -> Any
    end.

-spec playlists(handler(), esocial_id()) -> [playlist()]. % {{{1
playlists(#esocial{token=Token}=Handler, Id) ->
    Method = "audio.getAlbums",
    Args = [{owner_id, integer_to_binary(Id)},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, Audio} -> lists:map(fun(P) ->
                                         decode_playlist(P, Handler) 
                                 end,
                                 Audio);
        Any -> Any
    end.

-spec track(handler(), esocial_id()) -> track(). % {{{1
track(#esocial{token=Token}=Handler, Id) ->
    Method = "audio.get",
    Args = [
            {audio_ids, [integer_to_binary(Id)]},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, [Audio]} -> decode_audio(Audio);
        Any -> Any
    end.

-spec tracks(handler(), [esocial_id()]) -> [track()]. % {{{1
tracks(#esocial{token=Token}=Handler, IDs) ->
    Method = "audio.get",
    BinIDs = string:join([integer_to_list(ID) || ID <- IDs], ","),

    Args = [{audio_ids, BinIDs},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, Audios} -> lists:map(fun decode_audio/1, Audios);
        Any -> Any
    end.

-spec playlist_tracks(handler(), esocial_id()) -> [track()]. % {{{1
playlist_tracks(#esocial{token=Token}=Handler, PlaylistID) ->
    Method = "audio.get",
    Args = [
            {album_id, integer_to_binary(PlaylistID)},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, Audios} -> lists:map(fun decode_audio/1, Audios);
        Any -> Any
    end.

-spec user_tracks(handler(), esocial_id()) -> [track()]. % {{{1
user_tracks(#esocial{token=Token}=Handler, OwnerID) ->
    Method = "audio.get",
    Args = [
            {owner_id, integer_to_binary(OwnerID)},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, Audios} -> lists:map(fun decode_audio/1, Audios);
        Any -> Any
    end.

-spec call(Method, Args) -> Response when % {{{1
      Method :: iodata(),
      Args :: map() | [proplists:property()],
      Response :: map() | [proplists:property()].
call(Method, Args) -> 
    gen_server:call(?MODULE, {call, Method, Args}, infinity).

-spec get_access_token(Code, Redirect) -> map() when  % {{{1
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
init([Config]) -> % {{{1
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
handle_call({connect, RedirectURI, Scopes}, From, #state{app_id=AppID, secret=Secret}=State) -> % {{{1
    Scope = string:join(Scopes, ","),
    Return = lists:flatten(
               io_lib:format(
                 "~s/authorize?state=vk&client_id=~p&display=page&scope=~s&response_type=code&redirect_uri=~s",
                 [?BASE_AUTH_URL, AppID, Scope, RedirectURI])),
    {reply, Return, State};
handle_call({auth, Code, Redirect}, From, #state{app_id=AppID, secret=Secret}=State) -> % {{{1
    Args = [
            {client_id, AppID},
            {client_secret, Secret},
            {redirect_uri, Redirect},
            {code, Code}
           ],
    URL = hackney_url:make_url(?BASE_AUTH_URL, [<<"access_token">>], Args),
    hottub:cast(request, {From, URL}),
    {noreply, State};
handle_call({call, Method, Args}, From, #state{app_id=AppID}=State) -> % {{{1
    URL = hackney_url:make_url(?BASE_URL, [<<"method">>, Method], Args),
    hottub:cast(request, {From, URL}),
    {noreply, State};
handle_call(_Request, _From, State) -> % {{{1
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
parse_response(#{<<"response">> := [N | Data]}) when is_integer(N) ->  % {{{1
    {ok, Data};
parse_response(#{<<"response">> := Data}) ->  % {{{1
    {ok, Data};
parse_response(#{<<"error">> := #{<<"error_code">> := 14,  % {{{1
                                    <<"captcha_sid">> :=Sid,
                                    <<"captcha_img">> := Img
                                    }}) ->
    lager:warning("Captcha input needed for SID: ~p img: ~p", [Sid, Img]),
    {captcha, {Sid, Img}};
parse_response(#{<<"error">> := #{<<"error_code">> := 5}}) ->  % {{{1
    logout;
parse_response(#{<<"error">> := Err}) ->  % {{{1
    lager:warning("VK error: ~p", [Err]),
    {error, Err}.

decode_audio(#{<<"aid">> := AID, % {{{1
               <<"artist">> := Artist,
               %<<"album">> := Album,
               <<"title">> := Title,
               <<"duration">> :=Duration,
               <<"url">> := URL
              }) ->
    #esocial_track{
       id = AID,
       name = Title,
       artist = Artist,
       duration = Duration,
       uri = URL
      }.

decode_playlist(#{<<"album_id">> := AID, % {{{1
                  <<"title">> := Title
                 },
                Handler) ->
    Tracks = playlist_tracks(Handler, AID),
    #esocial_playlist{
       id = AID,
       name = Title, 
       tracks = [TID || #esocial_track{id=TID} <- Tracks]
      }.
