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
           name = {integer, 1, 65535},
           typical = 1,
           reloadable = false}},
       {address,
        #json_type{
           name = ip_address_port,
           typical = {{242,45,0,34}, 10000},
           reloadable = false}}]}].
