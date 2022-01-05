-module(gaia_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").
-include_lib("apptools/include/shorthand.hrl").

get() ->
    [{gaia,
      [{enabled,
        #json_type{
           name = bool,
           reloadable = false}},
       {'peer-name',
        #json_type{
           name = string,
           reloadable = false}},
       {'peer-id',
        #json_type{
           name = {integer, -1, 65535},
           typical = -1,
           reloadable = false}},
       {'gaia-port',
        #json_type{
           name = {integer, 1024, 65535},
           reloadable = false}},
       {'use-opus-codec',
        #json_type{
           name = bool,
           reloadable = false}},
       {'capture-pcm-name',
        #json_type{
           name = string,
           typical = <<"default">>,
           reloadable = false}},
       {'playback-pcm-name',
        #json_type{
           name = string,
           typical = <<"default">>,
           reloadable = false}},
       {peers,
        [[{name,
           #json_type{
              name = string,
              reloadable = false}},
          {id,
           #json_type{
              name = {integer, -1, 65535},
              reloadable = false}},
          {modes,
           [#json_type{
               name = string,
               transform =
                   fun(<<"direct">>) -> direct;
                      (<<"override-if-busy">>) -> override_if_busy;
                      (<<"ask">>) -> ask;
                      (<<"ignore">>) -> ignore;
                      (<<"mute">>) -> mute;
                      (<<"cleartext">>) -> cleartext;
                      (_) ->
                           throw(
                             {failed,
                              "Must be one of direct, override-if-busy, ask, ignore, mute or cleartext"})
                   end,
               reloadable = true}]}]]},
       {groups,
        [[{name,
           #json_type{
              name = string,
              reloadable = false}},
          {id,
           #json_type{
              name = {integer, -1, 65535},
              reloadable = false}},
          {modes,
           [#json_type{
               name = string,
               transform =
                   fun(<<"direct">>) -> direct;
                      (<<"override-if-busy">>) -> override_if_busy;
                      (<<"ask">>) -> ask;
                      (<<"ignore">>) -> ignore;
                      (<<"mute">>) -> mute;
                      (<<"cleartext">>) -> cleartext;
                      (_) ->
                           throw(
                             {failed,
                              "Must be one of direct, override-if-busy, ask, ignore, mute or cleartext"})
                   end,
               reloadable = true}]},
          {members,
           [#json_type{
               name = string,
               reloadable = true}]}]]}]}].
