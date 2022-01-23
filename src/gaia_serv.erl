-module(gaia_serv).
-export([start_link/5, stop/0]).
-export([get_status/0, set_status/1,
         mute/0, unmute/0,
         all_peers/0, all_groups/0,
         currently_talking_to/0, start_talking_to/1, stop_talking_to/1,
         get_by_id/1, get_by_name/1,
         generate_artificial_id/1,
         peer_wants_to_negotiate/2]).
-export([message_handler/1]).
-export_type([name/0, peer_name/0, group_name/0,
              id/0, peer_id/0, group_id/0,
              mode/0, options/0,
              group_type/0,
              session_key/0,
              status/0]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/gaia_serv.hrl").
-include("globals.hrl").

-type name() :: binary().
-type peer_name() :: name().
-type group_name() :: name().
-type id() :: non_neg_integer().
-type peer_id() :: id().
-type group_id() :: id().
%% FIXME: ask has not been implemented fully
-type mode() :: direct | ask | ignore.
-type options() :: [override_busy].
-type group_type() :: open | closed.
-type session_key() :: binary().
-type status() :: available | busy.

%%
%% Exported: start_link
%%

-spec start_link(binary(), peer_name(), peer_id(), inet:port_number(),
                 string()) ->
          serv:spawn_server_result().

start_link(GaiaDir, PeerName, PeerId, RestPort, PlaybackPcmName) ->
    ?spawn_server(
       fun(Parent) ->
               init(Parent, GaiaDir, PeerName, PeerId, RestPort,
                    PlaybackPcmName)
       end,
       fun initial_message_handler/1,
       #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: get_status
%%

-spec get_status() -> status().

get_status() ->
    serv:call(?MODULE, get_status).

%%
%% Exported: set_status
%%

-spec set_status(status()) -> ok.

set_status(Status) ->
    serv:call(?MODULE, {set_status, Status}).

%%
%% Exported: mute
%%

-spec mute() -> ok.

mute() ->
    serv:call(?MODULE, mute).

%%
%% Exported: unmute
%%

-spec unmute() -> ok.

unmute() ->
    serv:call(?MODULE, unmute).

%%
%% Exported: all_peers
%%

-spec all_peers() -> [#gaia_peer{}].

all_peers() ->
    serv:call(?MODULE, all_peers).

%%
%% Exported: all_groups
%%

-spec all_groups() -> [#gaia_group{}].

all_groups() ->
    serv:call(?MODULE, all_groups).

%%
%% Exported: currently_talking_to
%%

-spec currently_talking_to() -> [#gaia_peer{}|#gaia_group{}].

currently_talking_to() ->
    serv:call(?MODULE, currently_talking_to).

%%
%% Exported: start_talking_to
%%

-spec start_talking_to(id() | {name, name()}) ->
          ok | {error, no_such_id | no_such_name | already_talking_to}.

start_talking_to(IdOrName) ->
    serv:call(?MODULE, {start_talking_to, IdOrName}).

%%
%% Exported: stop_talking_to
%%

-spec stop_talking_to(id() | {name, name()} | all) ->
          ok | {error, no_such_id | no_such_name | not_talking_to}.

stop_talking_to(IdOrName) ->
    serv:call(?MODULE, {stop_talking_to, IdOrName}).

%%
%% Exported: get_by_id
%%

-spec get_by_id(id()) -> [#gaia_peer{}|#gaia_group{}].

get_by_id(Id) ->
    serv:call(?MODULE, {get_by_id, Id}).

%%
%% Exported: get_by_name
%%

-spec get_by_name(name()) -> [#gaia_peer{}|#gaia_group{}].

get_by_name(Name) ->
    serv:call(?MODULE, {get_by_name, Name}).

%%
%% generate_artificial_id
%%

-spec generate_artificial_id(peer_name()) -> peer_id().

generate_artificial_id(PeerName) when size(PeerName) < 5 ->
    binary:decode_unsigned(PeerName);
generate_artificial_id(PeerName) ->
    binary:decode_unsigned(binary:part(PeerName, {byte_size(PeerName), -4})).

%%
%% Exported: peer_wants_to_negotiate
%%

-spec peer_wants_to_negotiate(peer_id(), inet:port_number()) ->
          {ok, inet:port_number()} | {error, no_such_peer_id | not_available}.

peer_wants_to_negotiate(PeerId, RemotePort) ->
    serv:call(?MODULE, {peer_wants_to_negotiate, PeerId, RemotePort}).

%%
%% Server
%%

init(Parent, GaiaDir, PeerName, PeerId, RestPort, PlaybackPcmName) ->
    ?LOG_INFO("Gaia NIF is initializing..."),
    ok = gaia_nif:start(#{pcm_name => PlaybackPcmName,
                          playback_audio => ?PLAYBACK_AUDIO}),
    ?LOG_INFO("Gaia NIF has been initialized"),
    ok = nodis:set_node_info(
           #{gaia => #{peer_id => PeerId, rest_port => RestPort}}),
    ok = config_serv:subscribe(),
    {ok, NodisSubscription} = nodis_serv:subscribe(),
    ?LOG_INFO("Gaia server has been started"),
    {ok, #{parent => Parent,
           peer_id => PeerId,
           peer_name => PeerName,
           status => available,
           muted => false,
           db => new_db(GaiaDir),
           nodis_subscription => NodisSubscription}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, NeighbourWorkers} ->
            [NetworkSenderPid] =
                supervisor_helper:get_selected_worker_pids(
                  [gaia_network_sender_serv], NeighbourWorkers),
            {swap_message_handler, fun ?MODULE:message_handler/1,
             State#{network_sender_pid => NetworkSenderPid}}
    end.

message_handler(#{parent := Parent,
                  db := Db,
                  status := Status,
                  muted := Muted,
                  nodis_subscription := NodisSubscription,
                  network_sender_pid := NetworkSenderPid} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {call, From, get_status} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => get_status}),
            {reply, From, Status};
        {call, From, {set_status, NewStatus}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => {set_status, Status}}),
            ok = update_network(Db, NewStatus, Muted, NetworkSenderPid),
            {reply, From, ok, State#{status => NewStatus}};
        {call, From, mute} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => mute}),
            ok = update_network(Db, Status, Muted, NetworkSenderPid),
            {reply, From, ok, State#{muted => true}};
        {call, From, unmute} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => unmute}),
            ok = update_network(Db, Status, Muted, NetworkSenderPid),
            {reply, From, ok, State#{muted => false}};
        {call, From, {get_by_id, Id}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => {get_by_id, Id}}),
            {reply, From, db_get_by_id(Db, Id)};
        {call, From, {get_by_name, Name}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => {get_by_name, Name}}),
            {reply, From, db_get_by_name(Db, Name)};
        {call, From, all_peers} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => all_peers}),
            {reply, From, db_all_peers(Db)};
        {call, From, all_groups} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => all_groups}),
            {reply, From, db_all_groups(Db)};
        {call, From, currently_talking_to} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => currently_talking_to}),
            PeersAndGroups =
                db_foldl(
                  fun(#gaia_peer{talks_to = true} = Peer, Acc) ->
                          [Peer|Acc];
                     (#gaia_group{talks_to = true} = Group, Acc) ->
                          [Group|Acc];
                     (_, Acc) ->
                          Acc
                  end, [], Db),
            {reply, From, PeersAndGroups};
        {call, From, {start_talking_to, IdOrName}} ->
            case db_get_by(Db, IdOrName) of
                [#gaia_peer{talks_to = true}] ->
                    {reply, From, {error, already_talking_to}};
                [#gaia_group{talks_to = true}] ->
                    {reply, From, {error, already_talking_to}};
                [Peer] when is_record(Peer, gaia_peer) ->
                    UpdatedPeer = Peer#gaia_peer{talks_to = true},
                    true = db_insert(Db, UpdatedPeer),
                    ok = update_network(Db, Status, Muted, NetworkSenderPid),
                    {reply, From, ok};
                [Group] when is_record(Group, gaia_group) ->
                    UpdatedGroup = Group#gaia_group{talks_to = true},
                    true = db_insert(Db, UpdatedGroup),
                    ok = update_network(Db, Status, Muted, NetworkSenderPid),
                    {reply, From, ok};
                [] ->
                    case IdOrName of
                        {name, _} ->
                            {reply, From, {error, no_such_name}};
                        _ ->
                            {reply, From, {error, no_such_id}}
                    end
            end;
        {call, From, {stop_talking_to, all}} ->
            db_foldl(
              fun(#gaia_peer{talks_to = true} = Peer, Acc) ->
                      true = db_insert(Db, Peer#gaia_peer{talks_to = false}),
                      Acc;
                 (#gaia_group{talks_to = true} = Group, Acc) ->
                      true = db_insert(Db, Group#gaia_group{talks_to = false}),
                      Acc;
                 (_, Acc) ->
                      Acc
              end, undefined, Db),
            ok = update_network(Db, Status, Muted, NetworkSenderPid),
            {reply, From, ok};
        {call, From, {stop_talking_to, IdOrName}} ->
            case db_get_by(Db, IdOrName) of
                [#gaia_peer{talks_to = false}] ->
                    {reply, From, {error, not_talking_to}};
                [#gaia_group{talks_to = false}] ->
                    {reply, From, {error, not_talking_to}};
                [Peer] when is_record(Peer, gaia_peer) ->
                    UpdatedPeer = Peer#gaia_peer{talks_to = false},
                    true = db_insert(Db, UpdatedPeer),
                    ok = update_network(Db, Status, Muted, NetworkSenderPid),
                    {reply, From, ok};
                [Group] when is_record(Group, gaia_group) ->
                    UpdatedGroup = Group#gaia_group{talks_to = false},
                    true = db_insert(Db, UpdatedGroup),
                    ok = update_network(Db, Status, Muted, NetworkSenderPid),
                    {reply, From, ok};
                [] ->
                    case IdOrName of
                        {name, _} ->
                            {reply, From, {error, no_such_name}};
                        _ ->
                            {reply, From, {error, no_such_id}}
                    end
            end;
        {call, From, {peer_wants_to_negotiate, PeerId, RemotePort}} ->
            case db_get_peer_by_id(Db, PeerId) of
                [Peer] ->
                    true = db_insert(Db, Peer#gaia_peer{
                                           remote_port = RemotePort}),
                    ok = update_network(
                           Db, Status, Muted, NetworkSenderPid, false),
                    case db_get_peer_by_id(Db, PeerId) of
                        [#gaia_peer{local_port = undefined}] ->
                            {reply, From, {error, not_available}};
                        [_] ->
                            {reply, From, ok}
                    end;
                [] ->
                    {reply, From, {error, no_such_peer_id}}
            end;
        config_update ->
            ?LOG_DEBUG(#{module => ?MODULE, message => config_update}),
            true = sync_db(Db),
            ok = update_network(Db, Status, Muted, NetworkSenderPid),
            noreply;
        {nodis, NodisSubscription, {pending, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {pending, NodisAddress}}),
            noreply;
        {nodis, NodisSubscription, {up, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {up, NodisAddress}}),
            noreply;
        {nodis, NodisSubscription, {change, NodisAddress, Info}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         nodis => {change, NodisAddress, Info}}),
            case change_peer(Db, NodisAddress, Info) of
                ok ->
                    ok = update_network(Db, Status, Muted, NetworkSenderPid),
                    noreply;
                {error, Reason} ->
                    ?LOG_ERROR(#{module => ?MODULE, change_peer => Reason}),
                    noreply
            end;
        {nodis, NodisSubscription, {wait, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {wait, NodisAddress}}),
            noreply;
        {nodis, NodisSubscription, {down, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {down, NodisAddress}}),
            case down_peer(Db, NodisAddress) of
                ok ->
                    ok = update_network(Db, Status, Muted, NetworkSenderPid),
                    noreply;
                {error, Reason} ->
                    ?LOG_ERROR(#{module => ?MODULE, change_peer => Reason}),
                    noreply
            end;
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

update_network(Db, Status, Muted, NetworkSenderPid) ->
    update_network(Db, Status, Muted, NetworkSenderPid, _Negotiate = true).

update_network(Db, Status, Muted, NetworkSenderPid, Negotiate) ->
    case db_get_peer_by_name(Db, <<"*">>) of
        [#gaia_peer{talks_to = true} = WildcardPeer] ->
            update_network(Db, Status, Muted, NetworkSenderPid,
                           Negotiate, {true, WildcardPeer});
        _ ->
            update_network(Db, Status, Muted, NetworkSenderPid,
                           Negotiate, {false, undefined})
    end.

update_network(Db, Status, Muted, NetworkSenderPid, Negotiate, Wildcard) ->
    Destinations =
        case Muted of
            true ->
                [];
            false ->
                get_destinations(Db, Status, Wildcard)
        end,
    Sources = get_sources(Db, Status, Wildcard),
    ok = update_network_receiver(Db, Sources),
    case Negotiate of
        true ->
            ok = negotiate_with_peers(Db, lists:usort(Destinations ++ Sources));
        false ->
            skip_negotation
    end,
    update_network_sender(Db, NetworkSenderPid, Destinations).

get_destinations(_Db, busy, _Wildcard) ->
    [];
get_destinations(Db, _Status, Wildcard) ->
    db_foldl(
      fun(#gaia_peer{name = <<"*">>}, Acc) ->
              Acc;
         (#gaia_peer{id = PeerId} = Peer, Acc) ->
              case use_destination_peer(Wildcard, Peer) of
                  true ->
                      [{peer, PeerId}|Acc];
                  false ->
                      Acc
              end;
         (#gaia_group{id = GroupId, port = GroupPort} = Group, Acc) ->
              case use_destination_group(Group) of
                  true ->
                      [{group, GroupId, GroupPort}|Acc];
                  false ->
                      Acc
              end
      end, [], Db).

use_destination_peer(_Wildcard, #gaia_peer{nodis_address = undefined}) ->
    false;
use_destination_peer({UsesWildcard, _WildcardPeer},
                     #gaia_peer{ephemeral = Ephemeral,
                                talks_to = TalksTo}) ->
    if
        UsesWildcard andalso Ephemeral ->
            true;
        TalksTo ->
            true;
        true ->
            false
    end.

use_destination_group(#gaia_group{talks_to = TalksTo}) ->
    TalksTo.

get_sources(Db, Status, Wildcard) ->
    db_foldl(
      fun(#gaia_peer{name = <<"*">>}, Acc) ->
              Acc;
         (#gaia_peer{id = PeerId} = Peer, Acc) ->
              case use_source_peer(Wildcard, Peer) of
                  {true, Options} when Status == busy ->
                      case lists:member(override_busy, Options) of
                          true ->
                              [{peer, PeerId}|Acc];
                          false ->
                              Acc
                      end;
                  {true, _Options} ->
                      [{peer, PeerId}|Acc];
                  {false, _Options} ->
                      Acc
              end;
         (#gaia_group{id = GroupId,
                      options = Options,
                      port = GroupPort} = Group, Acc) ->
              case use_source_group(Group) of
                  true when Status == busy ->
                      case lists:member(override_busy, Options) of
                          true ->
                              [{group, GroupId, GroupPort}|Acc];
                          false ->
                              Acc
                      end;
                  true ->
                      [{group, GroupId, GroupPort}|Acc];
                  false ->
                      Acc
              end
      end, [], Db).

use_source_peer(_Wildcard, #gaia_peer{nodis_address = undefined}) ->
    {false, undefined};
use_source_peer({UsesWildcard, WildcardPeer},
                #gaia_peer{mode = Mode,
                           ephemeral = Ephemeral,
                           options = Options,
                           talks_to = TalksTo,
                           local_port = LocalPort,
                           remote_port = RemotePort}) ->
    if
        LocalPort == undefined andalso RemotePort /= undefined ->
            {true, Options};
        UsesWildcard andalso Ephemeral ->
            {true, WildcardPeer#gaia_peer.options};
        TalksTo ->
            {true, Options};
        not TalksTo andalso (Mode == direct orelse Mode == ask) ->
            {true, Options};
        true ->
            {false, undefined}
    end.

use_source_group(#gaia_group{talks_to = TalksTo}) ->
    TalksTo.

update_network_receiver(Db, Sources) ->
    ?LOG_INFO(#{module => ?MODULE, sources => Sources}),
    LocalPorts = gaia_nif:set_sources(Sources),
    ?LOG_INFO(#{module => ?MODULE, local_ports => LocalPorts}),
    lists:foreach(
      fun({{peer, PeerId}, NewLocalPort}) ->
              case db_get_peer_by_id(Db, PeerId) of
                  [#gaia_peer{local_port = NewLocalPort}] ->
                      ok;
                  [Peer] ->
                      db_insert(Db, Peer#gaia_peer{local_port = NewLocalPort})
              end;
         ({{group, GroupId}, NewGroupPort}) ->
              [#gaia_group{port = NewGroupPort}] =
                  db_get_group_by_id(Db, GroupId)
      end, LocalPorts).

negotiate_with_peers(_Db, []) ->
    ok;
negotiate_with_peers(Db, [{group, _GroupId, _GroupPort}|Rest]) ->
    negotiate_with_peers(Db, Rest);
negotiate_with_peers(Db, [{peer, PeerId}|Rest]) ->
    [#gaia_peer{nodis_address = {IpAddress, _SyncPort},
                rest_port = RestPort,
                local_port = LocalPort} = Peer] =
        db_get_peer_by_id(Db, PeerId),
    case gaia_rest_service:negotiate_with_peer(PeerId, {IpAddress, RestPort},
                                               LocalPort) of
        {ok, NewRemotePort} ->
            true = db_insert(Db, Peer#gaia_peer{remote_port = NewRemotePort}),
            negotiate_with_peers(Db, Rest);
        {error, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE,
                         {rest_service_client, negotiate} => Reason}),
            negotiate_with_peers(Db, Rest)
    end.

update_network_sender(Db, NetworkSenderPid, Destinations) ->
    DestinationAddresses =
        lists:foldl(
          fun({peer, PeerId}, Acc) ->
                  [#gaia_peer{nodis_address = {IpAddress, _SyncPort},
                              remote_port = RemotePort}] =
                      db_get_peer_by_id(Db, PeerId),
                  [{IpAddress, RemotePort}|Acc];
             ({group, GroupId}, Acc) ->
                  [#gaia_group{port = GroupPort,
                               members = Members}] =
                      db_get_group_by_id(Db, GroupId),
                  lists:foldl(
                    fun({PeerId, _PeerName}, MemberAddresses) ->
                            [#gaia_peer{
                                nodis_address = {IpAddress, _SyncPort}}] =
                                db_get_peer_by_id(Db, PeerId),
                            [{IpAddress, GroupPort}|MemberAddresses]
                    end, [], Members) ++ Acc
          end, [], Destinations),
    ?LOG_INFO(#{module => ?MODULE,
                destination_addresses => DestinationAddresses}),
    gaia_network_sender_serv:set_destination_addresses(
      NetworkSenderPid, DestinationAddresses).

change_peer(Db, NewNodisAddress, Info) ->
    MaxPeerId = math:pow(2, 32) - 1,
    case lists:keysearch(gaia, 1, Info) of
        {value, {gaia, #{peer_id := NewPeerId, rest_port := NewRestPort},
                 _PreviousGaiaInfo}}
          when is_integer(NewPeerId) andalso
               NewPeerId > 0 andalso
               NewPeerId =< MaxPeerId andalso
               is_integer(NewRestPort) andalso
               NewRestPort >= 1024 andalso
               NewRestPort =< 65535 ->
            case db_get_peer_by_id(Db, NewPeerId) of
                [#gaia_peer{nodis_address = NodisAddress,
                            rest_port = RestPort} = Peer]
                  when NodisAddress /= NewNodisAddress andalso
                       RestPort /= NewRestPort ->
                    UpdatedPeer =
                        Peer#gaia_peer{nodis_address = NewNodisAddress,
                                               rest_port = NewRestPort},
                    true = db_insert(Db, UpdatedPeer),
                    ok;
                [_] ->
                    ok;
                [] ->
                    EphemeralPeer =
                        #gaia_peer{
                           id = NewPeerId,
                           ephemeral = true,
                           nodis_address = NewNodisAddress,
                           rest_port = NewRestPort},
                    true = db_insert(Db, EphemeralPeer),
                    ok
                end;
        {value, {gaia, GaiaInfo, _PreviousGaiaInfo}} ->
            {error, {bad_gaia_info, GaiaInfo}};
        false ->
            {error, no_gaia_info}
    end.

down_peer(Db, NodisAddress) ->
    case db_get_peer_by_nodis_address(Db, NodisAddress) of
        [#gaia_peer{id = PeerId, ephemeral = true}] ->
            true = db_delete(Db, PeerId),
            ok;
        [Peer] ->
            true = db_insert(Db, Peer#gaia_peer{
                                   nodis_address = undefined,
                                   rest_port = undefined}),
            ok;
        [] ->
            {error, no_such_nodis_address}
    end.

%%
%% Peer and group database API
%%

new_db(GaiaDir) ->
    FilePath = filename:join([GaiaDir, ?MODULE]),
    #gaia_peer.id = #gaia_group.id, %% This must me true!
    {ok, DetsTab} =
        dets:open_file(?MODULE, [{file, ?b2l(FilePath)},
                                 {keypos, #gaia_peer.id}]),
    Tab = ets:new(?MODULE, [public, {keypos, #gaia_peer.id}]),
    Tab = dets:to_ets(DetsTab, Tab),
    Db = {Tab, DetsTab},
    ok = sync_with_config(Db, true),
    ?LOG_DEBUG(#{module => ?MODULE, db_created => ets:tab2list(Tab)}),
    Db.

sync_db({Tab, DetsTab} = Db) ->
    ok = sync_with_config(Db, false),
    DetsTab = ets:to_dets(Tab, DetsTab),
    ?LOG_DEBUG(#{module => ?MODULE,
                 db_synced_with_config => ets:tab2list(Tab)}),
    true.

sync_with_config(Db, _DeleteEphemeralPeers = true) ->
    db_delete_ephemeral_peers(Db),
    sync_with_config(Db, false);
sync_with_config(Db, _DeleteEphemeralPeers = false) ->
    %% Update existing peers and groups
    {NewConfigPeers, NewConfigGroups} =
        db_foldl(
          fun(#gaia_peer{id = Id} = Peer, {ConfigPeers, ConfigGroups}) ->
                  case lists:keytake(id, 1, ConfigPeers) of
                      {value, ConfigPeer, RemainingConfigPeers} ->
                          [Mode, Options] =
                              config:lookup_children([mode, options],
                                                     ConfigPeer),
                          UpdatedPeer =
                              Peer#gaia_peer{mode = Mode,
                                             options = Options},
                          true = db_insert(Db, UpdatedPeer),
                          {RemainingConfigPeers, ConfigGroups};
                      false ->
                          true = db_delete(Db, Id),
                          {ConfigPeers, ConfigGroups}
                  end;
             (#gaia_group{id = Id} = Group, {ConfigPeers, ConfigGroups}) ->
                  case lists:keytake(id, 1, ConfigGroups) of
                      {value, ConfigGroup, RemainingConfigGroups} ->
                          [Mode, Options, Members, Admin] =
                              config:lookup_children(
                                [mode, options, members, admin], ConfigGroup),
                          UpdatedGroup =
                              Group#gaia_group{
                                mode = Mode,
                                options = Options,
                                members = Members,
                                admin = Admin},
                          true = db_insert(Db, UpdatedGroup),
                          {ConfigPeers, RemainingConfigGroups};
                      false ->
                          true = db_delete(Db, Id),
                          {ConfigPeers, ConfigGroups}
                  end
          end, {config:lookup([gaia, peers]),
                config:lookup([gaia, groups])}, Db),
    %% Create new peers
    lists:foreach(
      fun(ConfigPeer) ->
              [Name, Id, Mode, Options] =
                  config:lookup_children([name, id, mode, options], ConfigPeer),
              Peer = #gaia_peer{
                        name = Name,
                        id = generate_id_if_needed(Id, Name),
                        mode = Mode,
                        options = Options},
              true = db_insert(Db, Peer)
      end, NewConfigPeers),
    %% Create new groups
    lists:foreach(
      fun(ConfigGroup) ->
              [Name, Id, Mode, Options, Port, Type, Members, Admin] =
                  config:lookup_children([name, id, mode, options, port, type,
                                          members, admin], ConfigGroup),
              Peer = #gaia_group{
                        name = Name,
                        id = generate_id_if_needed(Id, Name),
                        mode = Mode,
                        options = Options,
                        port = Port,
                        type = Type,
                        members = Members,
                        admin = Admin},
              true = db_insert(Db, Peer)
      end, NewConfigGroups),
    %% Resolve peer ids
    db_foldl(
      fun(#gaia_group{members = Members, admin = Admin} = Group, Acc) ->
              UpdatedGroup =
                  Group#gaia_group{
                    members = add_peer_id(Db, Members),
                    admin = get_peer_id(Db, Admin)},
              true = db_insert(Db, UpdatedGroup),
              Acc;
         (_, Acc) ->
              Acc
      end, ok, Db).

generate_id_if_needed(-1, Name) ->
    generate_artificial_id(Name);
generate_id_if_needed(Id, _Name) ->
    Id.

get_peer_id(Db, PeerName) ->
    case db_get_peer_by_name(Db, PeerName) of
        [#gaia_peer{id = PeerId}] ->
            PeerId;
        [] ->
            generate_artificial_id(config:lookup([gaia, 'peer-name']))
    end.

add_peer_id(_Db, []) ->
    [];
add_peer_id(Db, [<<"*">>|Rest]) ->
    [{-1, <<"*">>}|add_peer_id(Db, Rest)];
add_peer_id(Db, [PeerName|Rest]) ->
    case db_get_peer_by_name(Db, PeerName) of
        [#gaia_peer{id = PeerId}] ->
            [{PeerId, PeerName}|add_peer_id(Db, Rest)];
        [] ->
            PeerName = config:lookup([gaia, 'peer-name']),
            [{generate_artificial_id(PeerName), PeerName}|add_peer_id(Db, Rest)]
    end.

db_all_peers({Tab, _DetsTab}) ->
    ets:match_object(Tab, #gaia_peer{_ = '_'}).

db_all_groups({Tab, _DetsTab}) ->
    ets:match_object(Tab, #gaia_group{_ = '_'}).

db_insert({Tab, DetsTab}, PeerOrGroup) ->
    ok = dets:insert(DetsTab, PeerOrGroup),
    ets:insert(Tab, PeerOrGroup).

db_delete({Tab, DetsTab}, Id) ->
    ok = dets:delete(DetsTab, Id),
    ets:delete(Tab, Id).

db_get_by(Db, {name, Name}) ->
    db_get_by_name(Db, Name);
db_get_by(Db, Id) ->
    db_get_by_id(Db, Id).

db_get_by_id({Tab, _DetsTab}, Id) ->
    ets:lookup(Tab, Id).

db_get_by_name({Tab, _DetsTab}, Name) ->
    case ets:match_object(Tab, #gaia_peer{name = Name, _ = '_'}) of
	[] ->
            ets:match_object(Tab, #gaia_group{name = Name, _ = '_'});
        Peers ->
            Peers
    end.

db_get_peer_by_id({Tab, _DetsTab}, PeerId) ->
    ets:match_object(Tab, #gaia_peer{id = PeerId, _ = '_'}).

db_get_group_by_id({Tab, _DetsTab}, GroupId) ->
    ets:match_object(Tab, #gaia_group{id = GroupId, _ = '_'}).

db_get_peer_by_name({Tab, _DetsTab}, Name) ->
    ets:match_object(Tab, #gaia_peer{name = Name, _ = '_'}).

db_get_peer_by_nodis_address({Tab, _DetsTab}, NodisAddress) ->
    ets:match_object(Tab, #gaia_peer{nodis_address = NodisAddress, _ = '_'}).

db_foldl(Fun, Acc, {Tab, _DetsTab}) ->
    ets:foldl(Fun, Acc, Tab).

db_delete_ephemeral_peers({Tab, DetsTab}) ->
    ok = dets:match_delete(DetsTab, #gaia_peer{ephemeral = true}),
    ets:match_delete(Tab, #gaia_peer{ephemeral = true}).

%db_dump({Tab, _DetsTab}) ->
%    ets:tab2list(Tab).
