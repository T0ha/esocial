-export_type([
              handler/0,
              platform/0,
              esocial_id/0
             ]).
-type platform() :: vk | sound_cloud | spotify.
-type esocial_id() :: {platform(), integer()}.

-record(esocial,
        {
         platform :: platform(),
         redirect_uri :: binary(),
         module :: module(),
         user_id :: non_neg_integer(),
         token :: binary(),
         args = [] :: [proplists:property()]
        }).

-opaque handler() :: #esocial{}.

-record(esocial_profile, 
        {
         id :: esocial_id(),
         display_name :: binary() | undefined,
         birthdate :: calendar:datetime(),
         country :: binary(),
         profile_uri :: binary(),
         photo :: binary()
        }).

-record(esocial_playlist,
        {
         id :: esocial_id(),
         name :: binary(),
         owner :: esocial_id(),
         tracks :: [track()]
        }).

-record(esocial_track, 
        {
         id :: esocial_id(),
         name :: binary(),
         artist :: [esocial_id()],
         album :: esocial_id(),
         duration :: non_neg_integer(),
         uri :: binary()
        }).

-record(esocial_artist, 
        {
         id :: esocial_id(),
         name :: binary()
        }).

-type profile() :: #esocial_profile{}.
-type playlist() :: #esocial_playlist{}.
-type artist() :: #esocial_artist{}.
-type track() :: #esocial_track{}.
