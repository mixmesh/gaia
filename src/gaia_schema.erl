-module(gaia_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").
-include_lib("apptools/include/shorthand.hrl").

get() ->
    [{gaia,
      [{enabled,
        #json_type{
           name = bool,
           typical = false,
           reloadable = false}},
       {'gaia-id',
        #json_type{
           name = {integer, -1, 65535},
           typical = -1,
           reloadable = false}},
       {port,
        #json_type{
           name = {integer, 1024, 65535},
           typical = 2305,
           reloadable = false}},
       {'use-opus-codec',
        #json_type{
           name = bool,
           typical = true,
           reloadable = false}},
       {'capture-pcm-name',
        #json_type{
           name = string,
           typical = <<"plughw:0,0">>,
           reloadable = false}},
       {'playback-pcm-name',
        #json_type{
           name = string,
           typical = <<"plughw:0,0">>,
           reloadable = false}}]}].
