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
           name = {integer, -1, trunc(math:pow(2, 32) - 1)},
           typical = -1,
           reloadable = false}},
       {'rest-port',
        #json_type{
           name = {integer, 0, 65535},
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
        [[{id,
           #json_type{
              name = {integer, -1, trunc(math:pow(2, 32) - 1)},
              reloadable = false}},
          {name,
           #json_type{
              name = string,
              reloadable = false}},
          {mode,
           #json_type{
              name = atom,
              transform =
                  fun(Mode) ->
                          case lists:member(Mode, [direct, ask, ignore]) of
                              true ->
                                  Mode;
                              false ->
                                  throw(
                                    {failed,
                                     "Must be one of direct, ask or ignore"})
                       end
                  end,
              reloadable = true}},
          {options,
           [#json_type{
               name = atom,
               transform =
                   fun('override-busy') -> override_busy;
                      ('known-peers-only') -> known_peers_only;
                      (_) ->
                           throw({failed,
                                  "Must be override-busy or known-peers-only \
(can only be used by the wildcard peer)"})
                   end,
               untransform =
                   fun(override_busy) ->
                           'override-busy';
                      (known_peers_only) ->
                           'known-peers-only'
                   end,
               reloadable = true}]}]]},
       {groups,
        [[{id,
           #json_type{
              name = {integer, -1, trunc(math:pow(2, 32) - 1)},
              reloadable = false}},
          {name,
           #json_type{
              name = string,
              reloadable = false}},
          {public,
           #json_type{
              name = bool,
              reloadable = true}},
          {'multicast-ip-address',
           #json_type{
              name = ip_address,
              transform = fun({0, 0, 0, 0}) -> undefined;
                             (IpAddress) -> IpAddress
                          end,
              untransform = fun(undefined) -> {0, 0, 0, 0};
                               (IpAddress) -> IpAddress
                            end,
              reloadable = true}},
          {port,
           #json_type{
              name = {integer, 1024, 65535},
              reloadable = true}},
          {type,
           #json_type{
              name = atom,
              transform =
                  fun(Type) ->
                          case lists:member(Type, [open, closed]) of
                              true ->
                                  Type;
                              false ->
                                  throw(
                                    {failed, "Must be one of open or closed"})
                          end
                  end,
              reloadable = true}},
          {members,
           [#json_type{
               name = string,
               reloadable = true}]},
          {admin,
           #json_type{
              name = string,
              reloadable = true}}]]}]}].
