-module(esocial_soundcloud).

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
         search_tracks/2,
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
          auth :: binary(),
          secret = <<>> :: binary(),
          captcha ::  [proplists:property()],
          config = [] :: [proplists:property()]
               }).
-define(BASE_URL, <<"https://api.soundcloud.com">>).
-define(BASE_AUTH_URL, <<"https://soundcloud.com">>).

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
        #{<<"access_token">> := Token} ->
            Response = gen_server:call(?MODULE, {call, "me", Token, []}, infinity),
            case Response of
                #{<<"id">> := UID} ->
                    {ok, 
                     #esocial{
                        module=?MODULE,
                        platform=soundcloud,
                        user_id=UID,
                        token=Token
                       }};
                #{<<"error">> := Any} -> {error, Any}
            end;
        Any -> {error, Any}
    end.
        
-spec profile(handler(), esocial_id()) -> profile(). % {{{1
profile(#esocial{token=Token}=Handler, Id) ->
    Method = <<"/users/", (integer_to_binary(Id))/bytes>>,
    Args = [],
    Response = gen_server:call(?MODULE, {call, Method, Token, Args}, infinity),
    case parse_response(Response) of
        {ok, #{
           <<"id">> := PID,
           <<"full_name">> := Name,
           <<"permalink_url">> := URL,
           <<"avatar_url">> := Photo
          } = User} ->
            {ok, 
             #esocial_profile{
               id = PID,
               display_name = Name,
               birthdate  = maps:get(<<"birthdate">>, User, <<>>),
               country = maps:get(<<"country">>, User, <<>>),
               profile_uri = URL,
               photo = Photo
              }};
        Any -> Any
    end.

-spec profiles(handler(), [esocial_id()]) -> [profile()]. % {{{1
profiles(#esocial{token=Token}=Handler, IDs) ->
    Raw =[profile(Handler, Id) || Id <- IDs],
    lists:foldl(fun({ok, User}, A) ->
                        [User | A];
                   ({error, _}, A) -> A;
                   ({captcha, _}=C, _A) -> C
                end,
                [],
                Raw).

-spec playlists(handler(), esocial_id()) -> playlist(). % {{{1
playlists(#esocial{user_id=Id, token=Token}=Handler, Id) ->
    Method = "/me/playlists",
    Args = [],
    Response = gen_server:call(?MODULE, {call, Method, Token, Args}, infinity),
    lager:info("Response: ~p", [Response]),
    lists:map(fun decode_playlist/1, Response).

-spec track(handler(), esocial_id()) -> track(). % {{{1
track(#esocial{token=Token}=Handler, Id) ->
    Method = "tracks",
    Args = [
            {audio_ids, [integer_to_binary(Id)]},
            {access_token, Token}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Args}, infinity),
    case parse_response(Response) of
        {ok, [Audio]} -> decode_audio(Audio, Token);
        Any -> Any
    end.

-spec tracks(handler(), [esocial_id()]) -> [track()]. % {{{1
tracks(#esocial{token=Token}=Handler, IDs) ->
    Method = "/tracks",
    BinIDs = string:join([integer_to_list(ID) || ID <- IDs], ","),

    Args = [
            {ids, BinIDs}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Token, Args}, infinity),
    lists:flatten(lists:map(fun(A) -> decode_audio(A, Token) end, Response)).
    
-spec search_tracks(handler(), iodata()) -> [track()]. % {{{1
search_tracks(#esocial{token=Token}=Handler, Term) ->
    Method = "/tracks",

    Args = [
            {q, Term}
           ],
    Response = gen_server:call(?MODULE, {call, Method, Token, Args}, infinity),
    lists:flatten(lists:map(fun(A) -> decode_audio(A, Token) end, Response)).

-spec user_tracks(handler(), esocial_id()) -> [track()]. % {{{1
user_tracks(#esocial{token=Token, user_id=OwnerID}=Handler, OwnerID) ->
    Method = "/me/playlists",
    Args = [],
    Response = gen_server:call(?MODULE, {call, Method, Token, Args}, infinity),
    lager:info("Response: ~p", [Response]),
    lists:flatten(lists:map(fun(A) -> decode_playlist_tracks(A, Token) end, Response)).

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
    Auth = base64:encode(<<AppID/bytes, ":", Secret/bytes>>),
    AuthHeader = {<<"Authorization">>, <<"Basic: ", Auth/bytes>>},
    lager:info("Starting vk handler for ~p", [ AppID ]),
    {ok, #state{app_id=AppID, config=Config, secret=Secret, auth=AuthHeader}}.

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
    Scope = string:join(Scopes, " "),
    Return = lists:flatten(
               io_lib:format(
                 "~s/connect?client_id=~s&response_type=code&redirect_uri=~s&state=soundcloud",
                 [?BASE_AUTH_URL, AppID, RedirectURI])),
    {reply, Return, State};
handle_call({auth, Code, Redirect}, From, #state{auth=Auth, app_id=AppID, secret=Secret}=State) -> % {{{1
    Args = [
            {<<"client_id">>, AppID},
            {<<"client_secret">>, Secret},
            {<<"grant_type">>, <<"authorization_code">>},
            {<<"redirect_uri">>, Redirect},
            {<<"code">>, Code}
           ],
    URL = hackney_url:make_url(?BASE_URL, [<<"/oauth2/token">>], []),
    hottub:cast(request, {From, post, URL, [], {form, Args}}),
    {noreply, State};
handle_call({call, Method, Token, Args}, From, #state{app_id=AppID}=State) -> % {{{1
    Args1 = [{oauth_token, Token} | Args],
    URL = hackney_url:make_url(?BASE_URL, [<<"v1">>, Method], Args1),
    hottub:cast(request, {From, get, URL, [], []}),
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
    {error, Err};
parse_response(Data) ->  % {{{1
    {ok, Data}.


decode_audio(#{<<"track">> := Track}, Token) -> % {{{1
    decode_audio(Track, Token);
decode_audio(#{<<"id">> := AID, % {{{1
               <<"user">> := #{<<"username">> := Artist},
               <<"title">> := Title,
               <<"duration">> :=Duration,
               <<"stream_url">> := URL
              }, Token) ->
    #esocial_track{
       id = AID,
       name = Title,
       artist = Artist,
       %album = Album,
       duration = Duration div 1000,
       uri = <<URL/bytes, "?oauth_token=", Token/bytes>>
      }.

decode_playlist_tracks(#{ % {{{1
  <<"tracks">> := Tracks
 }, Token) ->
    [ decode_audio(Track, Token) || Track <- Tracks].

decode_playlist(#{ % {{{1
  <<"id">> := Id,
  <<"title">> := Name,
  <<"user">> := #{<<"id">> := Owner},
  <<"tracks">> := Tracks
 }) ->

    #esocial_playlist{
         id = Id, 
         name = Name,
         owner = Owner,
         tracks = [TID || #{<<"id">> := TID} <- Tracks]
        }.
